package Sub::Spec::Runner::Orderly;
BEGIN {
  $Sub::Spec::Runner::Orderly::VERSION = '0.06';
}
# ABSTRACT: Run a set of subs (with dependency ordering, order changing, etc)

use 5.010;
use strict;
use warnings;
use Log::Any '$log';

use Moo;
use Sub::Spec::Utils; # temp, for _parse_schema


# {SUBNAME => {done=>BOOL, spec=>SPEC, fldepends=>FLATTENED_DEPENDS, ...}, ...}
has _sub_data  => (is => 'rw', default=>sub{{}});

# [SUBNAME, ...], the list of subroutine names in the order of execution
has _sub_list  => (is => 'rw', default=>sub{[]});

# for subroutines to share data
has _stash     => (is => 'rw', default=>sub{{}});

# index to _sub_list, the current running subroutine
has _i         => (is => 'rw');

# shorter way to insert custom code rather than subclassing
has _pre_run   => (is => 'rw');
has _post_run  => (is => 'rw');
has _pre_sub   => (is => 'rw');
has _post_sub  => (is => 'rw');



has args => (is => 'rw');


has load_modules => (is => 'rw', default=>sub{1});



sub __parse_schema {
    Sub::Spec::Utils::_parse_schema(@_);
}

# flatten depends clauses by flattening 'and' clauses, effectively change this
# depends clause:
#
#  {
#   sub => 's1',
#   and => [
#     {sub => 's1'},
#     {deb => {d1=>0, d2=>'>= 0', d3=>'= 1'},
#     {and => [{sub => 's2'}, {sub=>'s3'}, {deb=>{d3=>0}}],
#   ],
#  },
#
# into:
#
#  {
#   sub => ['s1', 's2', 's3'], # also uniquify sub names
#   deb => [{d1],
#  }
#
# dies if 'any' or 'none' clauses are encountered.
sub __flatten_depends {
    my ($depends, $res) = @_;
    $res //= {};
    while (my ($k, $v) = each %$depends) {
        if ($k =~ /^(any|none)$/) {
            die "Can't handle '$_' depends clause";
        } elsif ($k eq 'and') {
            __flatten_depends($_, $res) for @$v;
        } else {
            $res->{$k} //= [];
            next if $k eq 'sub' && $v ~~ @{ $res->{$k} };
            push @{ $res->{$k} }, $v;
        }
    }
    $res;
}

# add main:: if unqualified
sub __normalize_subname {
    my ($subname) = @_;
    $subname =~ /.+::.+/ ? $subname : "main::$subname";
}


sub get_spec {
    my ($self, $subname) = @_;
    my ($module, $sub) = $subname =~ /(.+)::(.+)/;
    no strict 'refs';
    my $ms = \%{"$module\::SPEC"};
    $ms->{$sub};
}


sub add {
    my ($self, $subname) = @_;
    $subname = __normalize_subname($subname);
    return if $self->_sub_data->{$subname};
    $log->tracef("-> add(%s)", $subname);

    # get the spec from modules
    my ($module, $sub);
    $subname =~ /(.+)::(.+)/;
    ($module, $sub) = ($1, $2);
    if ($self->load_modules) {
        my $modulep = $module;
        $modulep =~ s!::!/!g; $modulep .= ".pm";
        die "Cannot load module $module: $@\n"
            unless eval { require $modulep };
    }

    my $spec = $self->get_spec($subname);
    die "Can't find spec in \$$module\::SPEC{$sub}\n"
        unless $spec;

    my $fldeps = {};
    if ($spec->{depends}) { __flatten_depends($spec->{depends}, $fldeps) }
    $log->tracef("fldeps for $subname = %s", $fldeps);
    my $subdata = {
        name      => $subname,
        spec      => $spec,
        fldeps    => $fldeps,
    };
    $self->_sub_data->{$subname} = $subdata;
    push @{ $self->_sub_list }, $subname;

    if ($fldeps->{sub}) {
        $self->add($_) for @{ $fldeps->{sub} }
    }

    $log->trace("<- add()");
    # return order number
    scalar @{ $self->_sub_list };
}


sub order_by_dependencies {
    $log->tracef("-> order_by_dependencies()");
    require Algorithm::Dependency::Ordered;
    require Algorithm::Dependency::Source::HoA;

    my ($self) = @_;
    my %deps;
    while (my ($sn, $sd) = each %{$self->{_sub_data}}) {
        $deps{$sn} //= [];
        my $sub_deps = $sd->{fldeps}{sub};
        push @{ $deps{$sn} }, @$sub_deps if $sub_deps;
    }

    my $ds  = Algorithm::Dependency::Source::HoA->new(\%deps);
    my $ado = Algorithm::Dependency::Ordered->new(
        source   => $ds,
        selected => []
    );
    unless ($ado) {
        $log->error("Failed to set up dependency algorithm");
        return;
    }

    my $subs = $ado->schedule_all;
    unless (ref($subs) eq 'ARRAY') {
        return;
    }

    $self->add($_) for @$subs;

    $self->_sub_list($subs);
    1;
}


sub todo_subs {
    my ($self) = @_;
    [ grep {!$self->_sub_data->{done}} @{$self->_sub_list} ];
}


sub done_subs {
    my ($self) = @_;
    [ grep {$self->_sub_data->{done}} @{$self->_sub_list} ];
}

sub _log_running_sub {
    my ($self, $subname) = @_;
    $log->infof("Running %s ...", $self->format_subname($subname));
}


sub run {
    my ($self, %opts) = @_;
    $log->tracef("<- ".__PACKAGE__."::run(%s)", \%opts);

    return [400, "No subroutines to run"] unless @{ $self->_sub_list };
    if ($opts{order_by_dependencies} // 1) {
        return [412, "Cannot resolve dependencies, please check for circulars"]
            unless $self->order_by_dependencies;
    }
    return [412, "pre_run() didn't return true"] unless $self->pre_run;

    my $num_success_runs = 0;
    my $num_failed_runs  = 0;
    my %success_subs;
    my %failed_subs;
    my $res;

    my $use_last_res_status;
  RUN:
    while (1) {
        $self->{_i} = -1;
        my $some_not_done;
        my $jumped;
        while (1) {
            $self->{_i}++ unless $jumped;
            $jumped = 0;
            last unless $self->{_i} < @{ $self->_sub_list };
            my $subname = $self->_sub_list->[ $self->{_i} ];
            my $sd      = $self->_sub_data->{$subname};
            next if $sd->{done};

            $some_not_done++;
            $self->_log_running_sub($subname);

            my $orig_i = $self->{_i};
            unless ($self->pre_sub($subname)) {
                $res = [500, "pre_sub(%s) didn't return true"];
                $use_last_res_status++;
                last RUN;
            }
            next if $sd->{done}; # pre_sub might skip this sub
            $jumped = $orig_i != $self->{_i};

            if ($jumped) {
                last unless $self->{_i} < @{ $self->_sub_list };
            }

            $orig_i = $self->{_i};
            $res = $self->_run_sub($subname);
            $sd->{res} = $res;
            if ($self->success_res($res)) {
                $num_success_runs++;
                $success_subs{$subname}++;
                delete ($failed_subs{$subname});
            } else {
                $num_failed_runs++;
                $failed_subs{$subname}++;
                delete ($success_subs{$subname});
                unless ($opts{ignore_errors}) {
                    $use_last_res_status = 1;
                    last RUN;
                }
            }
            $self->done($subname, 1);
            $jumped = $orig_i != $self->{_i};

            if ($jumped) {
                last unless $self->{_i} < @{ $self->_sub_list };
            }

            $orig_i = $self->{_i};
            unless ($self->post_sub($subname)) {
                $res = [500, "post_sub(%s) didn't return true"];
                $use_last_res_status++;
                last RUN;
            }
            $jumped = $orig_i != $self->{_i};
        }
        last unless $some_not_done;
    }

    unless ($self->post_run) {
        $res = [500, "post_run() didn't return true"];
        $use_last_res_status = 1;
    }

    my $num_subs         = scalar(@{$self->_sub_list});
    my $num_success_subs = scalar(keys %success_subs);
    my $num_failed_subs  = scalar(keys %failed_subs );
    $res->[2] = {
        num_success_runs   => $num_success_runs,
        num_failed_runs    => $num_failed_runs,
        num_runs           => $num_success_runs+$num_failed_runs,
        num_success_subs   => $num_success_subs,
        num_failed_subs    => $num_failed_subs,
        num_subs           => $num_subs,
        num_run_subs       => $num_success_subs+$num_failed_subs,
        num_skipped_subs   => $num_subs - ($num_success_subs+$num_failed_subs),
    };
    unless ($use_last_res_status) {
        if ($num_success_subs) {
            if ($num_failed_subs) {
                $res->[0] = 200;
                $res->[1] = "Some failed";
            } else {
                $res->[0] = 200;
                $res->[1] = "All succeeded";
            }
        } else {
            $res->[0] = 500;
            $res->[1] = "All failed";
        }
    }
    $log->tracef("<- ".__PACKAGE__."::run(), res=%s", $res);
    $res;
}

sub _run_sub {
    my ($self, $subname) = @_;
    $log->tracef("-> _run_sub(%s)", $subname);
    my $res;
    eval {
        my ($module, $sub) = $subname =~ /(.+)::(.+)/;
        my $fref = \&{"$module\::$sub"};
        unless ($fref) {
            $res = [500, "No subroutine \&$subname defined"];
            last;
        }

        my $sd = $self->_sub_data->{$subname};

        my %args;
        my $args = $self->args // {};
        for (keys %$args) {
            $args{$_} = $args->{$_} if !$sd->{spec}{args} ||
                $sd->{spec}{args}{$_};
        }
        $log->tracef("-> %s(%s)", $subname, \%args);
        $args{-runner} = $self;

        $res = $fref->(%args);
        $log->tracef("<- %s(), res=%s", $subname, $res);
    };
    $res = [500, "Died: $@"] if $@;
    $log->tracef("<- _run_sub(%s), res=%s", $subname, $res);
    $res;
}


sub format_subname {
    $_[1];
}


sub success_res {
    my ($self, $res) = @_;
    $res->[0] >= 200 && $res->[0] <= 399;
}


sub pre_run {
    my $self = shift;
    !$self->_pre_run || $self->_pre_run->($self, @_);
}


sub pre_sub {
    my $self = shift;
    !$self->_pre_sub || $self->_pre_sub->($self, @_);
}


sub post_sub {
    my $self = shift;
    !$self->_post_sub || $self->_post_sub->($self, @_);
}


sub post_run {
    my $self = shift;
    !$self->_post_run || $self->_post_run->($self, @_);
}


sub done {
    my ($self, $subname0, $newval) = @_;

    my @subnames;
    if (ref($subname0) eq 'Regexp') {
        @subnames = grep {/$subname0/} @{$self->_sub_list};
    } else {
        push @subnames, __normalize_subname($subname0);
    }

    my $oldval;
    for my $subname (@subnames) {
        unless ($self->_sub_data->{$subname}) {
            $log->warn("Unknown subroutine $subname in set, ignored");
            next;
        }

        $oldval = $self->_sub_data->{$subname}{done};
        if (defined($newval)) {
            $self->_sub_data->{$subname}{done} = $newval;
        }
    }
    $oldval;
}


sub skip {
    my ($self, $subname) = @_;
    $self->done($subname, 1);
}


sub skip_all {
    my ($self) = @_;
    $self->done(qr/.*/, 1);
}


sub repeat {
    my ($self, $subname) = @_;
    $self->done($subname, 0);
}


sub repeat_all {
    my ($self) = @_;
    $self->done(qr/.*/, 1);
}

# return subname and all others which directly/indirectly depends on it
sub _find_dependants {
    my ($self, $subname, $res) = @_;
    $res //= [$subname];
    my $sd = $self->_sub_data;
    for (keys %$sd) {
        next if $_ ~~ @$res;
        if ($subname ~~ @{ $sd->{$_}{fldeps}{sub} // [] }) {
            push @$res, $_;
            $self->_find_dependants($_, $res);
        }
    }
    $res;
}


sub branch_done {
    my ($self, $subname, $newval) = @_;
    $subname = __normalize_subname($subname);
    unless ($self->_sub_data->{$subname}) {
        $log->warn("Unknown subroutine $subname in set, branch_done ignored");
        return;
    }
    my $sn = $self->_find_dependants($subname);
    for (@$sn) {
        $self->_sub_data->{$_}{done} = $newval;
    }
}


sub jump {
    my ($self, $subname) = @_;
    $subname = __normalize_subname($subname);
    unless ($self->_sub_data->{$subname}) {
        $log->warn("Unknown subroutine $subname in set, jump ignored");
        return;
    }
    my $sl = $self->_sub_list;
    for my $i (0..@$sl-1) {
        if ($sl->[$i] eq $subname) {
            $self->_i($i);
            last;
        }
    }
}


sub stash {
    my ($self, $key, $newval) = @_;
    my $oldval = $self->_stash->{$key};
    if (defined($newval)) {
        $self->_stash->{$key} = $newval;
    }
    $oldval;
}

1;


=pod

=head1 NAME

Sub::Spec::Runner::Orderly - Run a set of subs (with dependency ordering, order changing, etc)

=head1 VERSION

version 0.06

=head1 SYNOPSIS

In YourModule.pm:

 package YourModule;

 use 5.010;
 our %SPEC;

 $SPEC{a} = { depends => {sub=>'b'}, ... };
 sub a { my %args = @_; say "a"; [200, "OK"] }

 $SPEC{b} = { depends => {sub=>'c'}, ... };
 sub b { my %args = @_; say "b"; [200, "OK"] }

 $SPEC{c} = { depends => {and=>[{sub=>'d'}, {sub=>'e'}]}, ... };
 sub c { my %args = @_; say "c"; [200, "OK"] }

 $SPEC{d} = { depends => {sub=>'e'}, ... };
 sub d { my %args = @_; say "d"; [200, "OK"] }

 $SPEC{e} = { ... };
 sub e { my %args = @_; say "e"; [200, "OK"] }

In main module:

 use Sub::Spec::Runner::Orderly;

 my $runner = Sub::Spec::Runner::Orderly->new(load_modules=>0);
 $runner->add('YourModule::a');
 $runner->run;

Will output:

 e
 d
 c
 b
 a

=head1 DESCRIPTION

This class can run a set of subroutines along with their dependencies (read from
sub spec's 'depends' clause) according to the dependency tree order. During the
run, subroutines can instruct the runner to skip/repeat some subroutines, jump
to another subroutine, or even add more subroutines and reorder the execution.
The runner will finish after all subroutines have been run (or skipped).

This module uses L<Log::Any> logging framework. Use something like
L<Log::Any::App>, etc to see more logging statements for debugging.

This module uses L<Moo> for object system.

=head1 ATTRIBUTES

=head2 args => HASHREF

Arguments to pass to each subroutine. Note that each argument will only be
passed if the 'args' clause in subroutine spec specifies that the sub accepts
that argument, or if subroutine doesn't have an 'args' clause. Example:

 package Foo;

 our %SPEC;
 $SPEC{sub0} = {};
 sub sub0 { ... }

 $SPEC{sub1} = {args=>{}};
 sub sub1 { ... }

 $SPEC{sub2} = {args=>{foo=>"str"}};
 sub sub2 { ... }

 $SPEC{sub3} = {args=>{bar=>"str"}};
 sub sub2 { ... }

 $SPEC{sub4} = {args=>{foo=>"str", bar=>"str"}};
 sub sub4 { ... }

 package main;
 use Sub::Spec::Runner::Orderly;

 my $runner = Sub::Spec::Runner::Orderly->new(sub_args => {foo=>1, foo=>2});
 $runner->add("Foo::sub$_") for (1 2 3 4);
 $runner->run;

Then only sub0 and sub4 will receive 'foo' and 'bar' args. sub1 won't receive
any arguments, sub2 will only receive 'foo', sub3 will only receive 'bar'.

=head2 load_modules => BOOL

Whether to load (require()) modules when required, default is yes.

=head1 METHODS

=head2 $runner->get_spec($subname) => SPEC

Get spec for sub named $subname. Will be called by add(). Can be overriden to
provide your own specs other than from %SPECS package variables.

=head2 $runner->add($subname)

Add subroutine to the set of subroutines to be run. Example:

 $runner->add('Foo::bar');

Will also automatically add all dependencies.

=head2 $runner->order_by_dependencies()

Reorder set of subroutines by dependencies. Normally need not be called manually
since it will be caled by run() prior to running subroutines, but might be
useful if you want to add more subroutines in the middle of a run and then
reorder.

=head2 $runner->todo_subs() => ARRAYREF

Return the current list of subroutine names not yet runned, in order. Previously
run subroutines can belong to this list again if repeat()-ed.

=head2 $runner->done_subs() => ARRAYREF

Return the current list of subroutine names already run, in order. Never-run
subroutines can belong to this list too if skip()-ed.

=head2 $runner->run(%opts) => [STATUSCODE, ERRMSG, RESULT]

Options: ignore_errors => BOOL (default false), order_by_dependencies => BOOL
(default true).

Run the set of subroutines previously added by add(). Will return status code
400 if there are no subroutines to run.

Prior to running, 'depends' clause in each subroutine's spec will be read and
resolved first to add depended subroutines and change the order of execution
according to dependency tree. Will return 412 if dependency cannot be resolved.

Then it will call pre_run(), which you can override. pre_run() must return true,
or run() will immediately return with 412 error.

Then it will run each subroutine successively and store its result. Each
subroutine will be called with arguments specified in 'args', with one extra
special argument, '-runner' which is the runner object. Prior to running a
subroutine, pre_sub() will be called. It must return true, or run() will
immediately return with 500 error.

The subroutine being run can see the status/result of other subroutines by
calling $runner->done(), $runner->result(). It can share data by using
$runner->stash(). It can also change the ordering or repeat/skip some
subroutines by calling $runner->done(), skip(), skip_all(), repeat(),
repeat_all(). It can jump to other subroutines using $runner->jump(). See the
respective method documentation for more detail.

After running a subroutine, post_sub() will be called. It must return true, or
run() will immediately return with 500 error.

If ignore_errors is not set to true, then if the subroutine a non-success
result, run() will immediately exit with that result. The meaning of
subroutine's success can be changed by overriding success_res() (by default, all
2xx and 3xx are considered success).

After all subroutines have been run (or skipped), run() will call post_run()
which must return true or otherwise run() will immediately exit with 500 status.

After that, run() will return the summary. It will return status 200 if there
are at least one subroutine returning success, or 500 otherwise.

=head2 $runner->format_subname($subname) => STR

Can be used to format info log message: "Running XXX ..." when about to run a
subroutine inside run(). Default is "Running Package::bar ..." (just the
subname)

=head2 $runner->success_res($res) => BOOL

By default, all responses with 2xx and 3xx are assumed as a success. You can
override this.

=head2 $runner->pre_run() => BOOL

See run() for more details. Can be overridden by subclass.

=head2 $runner->pre_sub() => BOOL

See run() for more details. Can be overridden by subclass.

=head2 $runner->post_sub() => BOOL

See run() for more details. Can be overridden by subclass.

=head2 $runner->post_run() => BOOL

See run() for more details. Can be overridden by subclass.

=head2 $runner->done(SUBNAME[, VALUE]) => OLDVAL

If VALUE is set, set a subroutine to be done/not done. Otherwise will return the
current done status of SUBNAME.

SUBNAME can also be a regex, which means all subroutines matching the regex. The
last SUBNAME's current done status will be returned.

=head2 $runner->skip(SUBNAME)

Alias for done(SUBNAME, 1).

=head2 $runner->skip_all()

Alias for skip(qr/.*/, 1).

=head2 $runner->repeat(SUBNAME)

Alias for done(SUBNAME, 0).

=head2 $runner->repeat_all()

Alias for repeat(qr/.*/, 1).

=head2 $runner->branch_done(SUBNAME, VALUE)

Just like done(), except that will set SUBNAME *and all its dependants*.
Example: if a depends on b and b depends on c, then doing branch_done(c, 1) will
also set a & b as done.

SUBNAME must be a string and not regex.

=head2 $runner->jump($subname)

Jump to another subname. Can be called in pre_sub() or inside subroutine or
post_sub().

=head2 $runner->stash(NAME[, VALUE]) => OLDVAL

Get/set stash data. This is a generic place to share data between subroutines
being run.

=head1 SEE ALSO

L<Sub::Spec>

L<Sub::Spec::Clause::depends>

=head1 AUTHOR

Steven Haryanto <stevenharyanto@gmail.com>

=head1 COPYRIGHT AND LICENSE

This software is copyright (c) 2011 by Steven Haryanto.

This is free software; you can redistribute it and/or modify it under
the same terms as the Perl 5 programming language system itself.

=cut


__END__

