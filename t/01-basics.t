#!perl

use 5.010;
use strict;
use warnings;
use Log::Any '$log';
use Test::More 0.96;

use Capture::Tiny qw(capture);
use Sub::Spec::Runner::Orderly;

package Foo;
use 5.010;
our %SPEC;

$SPEC{a} = {depends=>{sub=>"Foo::b"}, args=>{alt=>"bool"}};
sub a {
    my %args=@_;
    print "A".($args{alt} ? "x" : "");
    [200, "OK", "apple"];
}
$SPEC{b} = {depends=>{and=>[{sub=>"Foo::c"},{sub=>"Foo::d"}]}};
sub b {
    my %args=@_;
    print "B".($args{alt} ? "x" : "");
    [200, "OK", "banana"];
}
$SPEC{c} = {depends=>{and=>[{sub=>"Foo::d"}, {sub=>"Foo::e"}]},
            args=>{alt=>"bool"}};
sub c {
    my %args=@_;
    print "C".($args{alt} ? "x" : "");
    [200, "OK", "cherry"];
}
$SPEC{d} = {depends=>{sub=>"Foo::e"}, args=>{}}; # won't supplied with args
sub d {
    my %args=@_;
    print "D".($args{alt} ? "x" : "");
    [200, "OK", "date"];
}
$SPEC{e} = {};
sub e {
    print "E";
    [304, "OK", "eggplant"];
}

$SPEC{read_ctx} = {depends=>{sub=>"Foo::a"}};
sub read_ctx {
    my %args=@_;
    my $ctx=$args{-ctx};
    my $res_a = $ctx->sub_res("Foo::a");
    #use Data::Dump qw(dump); open F, ">>/tmp/ctx"; print F dump($ctx); close F;
    if ($ctx->sub_res("Foo::a")->[2] eq 'avocado' &&
            $ctx->sub_res("Foo::b")->[2] eq 'blueberry') {
        return [200, "OK"];
    } else {
        return [500, "Failed"];
    }
}

# for testing ignore_errors
$SPEC{i} = {depends=>{sub=>"Foo::j"}};
sub i {
    print "I";
    [304, "OK"];
}
$SPEC{j} = {};
sub j {
    print "J";
    [450, "Failed"];
}

$SPEC{circ1} = {depends=>{sub=>"Foo::circ2"}};
sub circ1 {
    [200, "OK"];
}
$SPEC{circ2} = {depends=>{sub=>"Foo::circ1"}};
sub circ2 {
    [200, "OK"];
}

$SPEC{z} = {depends=>{sub=>"nonexisting"}};
sub z {
    [200, "OK"];
}

package Bar;
sub a { [200, "OK"] }
sub b { [200, "OK"] }

package main;

our %SPEC;
$SPEC{x} = {};
sub x {}

test_run(
    name          => 'add(x) becomes add(main::x)',
    subs          => ['x'],
    check_runner_before_run => sub {
        my ($runner) = @_;
        $runner->_sub_list->[0] eq 'main::x';
    }
);

test_run(
    name          => 'no subs',
    subs          => [],
    status        => 400,
);
test_run(
    name          => 'single sub',
    subs          => ['Foo::a'],
    status        => 200,
    num_runs      => 5, num_success_runs => 5, num_failed_runs  => 0,
    num_subs      => 5, num_success_subs => 5, num_failed_subs  => 0,
    num_run_subs  => 5, num_skipped_subs => 0,
    output_re     => qr/^EDCBA$/,
    more_tests    => sub {
        my ($runner) = @_;
        is_deeply($runner->_find_dependants('Foo::c'),
                  ['Foo::c', 'Foo::b', 'Foo::a'],
                  "_find_dependants 1");
        my $a = $runner->stash("a");
        ok(!$a, "stash default to undef");
        $a = $runner->stash("a", 1);
        ok(!$a, "stash returns old value");
        $a = $runner->stash("a");
        is($a, 1, "stash can set value");
    },
);
test_run(
    name          => 'single sub (no dependency)',
    subs          => ['Foo::e'],
    status        => 200,
    num_runs      => 1, num_success_runs => 1, num_failed_runs  => 0,
    num_subs      => 1, num_success_subs => 1, num_failed_subs  => 0,
    num_run_subs  => 1, num_skipped_subs => 0,
    output_re     => qr/^E$/,
);

test_run(
    name          => 'multiple subs',
    subs          => ['Foo::d', 'Foo::c'],
    status        => 200,
    num_runs      => 3, num_success_runs => 3, num_failed_runs  => 0,
    num_subs      => 3, num_success_subs => 3, num_failed_subs  => 0,
    num_run_subs  => 3, num_skipped_subs => 0,
    output_re     => qr/^EDC$/,
);

test_run(
    name          => 'args',
    subs          => ['Foo::a'],
    args          => {alt=>1},
    status        => 200,
    num_runs      => 5, num_success_runs => 5, num_failed_runs  => 0,
    num_subs      => 5, num_success_subs => 5, num_failed_subs  => 0,
    num_run_subs  => 5, num_skipped_subs => 0,
    output_re     => qr/^EDCxBxAx$/,
);

test_run(
    name          => 'cant resolve deps (circular)',
    subs          => ['Foo::circ1'],
    status        => 412,
);
test_run(
    name          => 'cant resolve deps (missing dep)',
    subs          => ['Foo::z'],
    add_dies      => 1,
);

test_run(
    name          => 'ignore_errors off',
    subs          => ['Foo::i'],
    status        => 450,
    num_runs      => 1, num_success_runs => 0, num_failed_runs  => 1,
    num_subs      => 2, num_success_subs => 0, num_failed_subs  => 1,
    num_run_subs  => 1, num_skipped_subs => 1,
    output_re     => qr/J/,
);
test_run(
    name          => 'ignore_errors on',
    subs          => ['Foo::i'],
    ignore_errors => 1,
    status        => 200,
    num_runs      => 2, num_success_runs => 1, num_failed_runs  => 1,
    num_subs      => 2, num_success_subs => 1, num_failed_subs  => 1,
    num_run_subs  => 2, num_skipped_subs => 0,
    output_re     => qr/JI/,
);
test_run(
    name          => 'ignore_errors on (all failed)',
    subs          => ['Foo::j'],
    ignore_errors => 1,
    status        => 500,
    num_runs      => 1, num_success_runs => 0, num_failed_runs  => 1,
    num_subs      => 1, num_success_subs => 0, num_failed_subs  => 1,
    num_run_subs  => 1, num_skipped_subs => 0,
    output_re     => qr/J/,
);

test_run(
    name          => 'pre_run',
    runner_args   => {_pre_run=>sub {0}},
    subs          => ['Foo::a'],
    status        => 412,
);

test_run(
    name          => 'post_run',
    runner_args   => {_post_run=>sub {0}},
    subs          => ['Foo::a'],
    status        => 500,
    num_runs      => 5, num_success_runs => 5, num_failed_runs  => 0,
    num_subs      => 5, num_success_subs => 5, num_failed_subs  => 0,
    num_run_subs  => 5, num_skipped_subs => 0,
    #output_re     => qr/EDCBA/,
);

test_run(
    name          => 'pre_sub',
    runner_args   => {_pre_sub=>sub {
                          my($self, $subname) = @_;
                          $subname eq 'Foo::c' ? 0:1;
                      }},
    subs          => ['Foo::a'],
    status        => 500,
    num_runs      => 2, num_success_runs => 2, num_failed_runs  => 0,
    num_subs      => 5, num_success_subs => 2, num_failed_subs  => 0,
    num_run_subs  => 2, num_skipped_subs => 3,
);

test_run(
    name          => 'post_sub',
    runner_args   => {_post_sub=>sub {
                          my($self, $subname) = @_;
                          $subname eq 'Foo::c' ? 0:1;
                      }},
    subs          => ['Foo::a'],
    status        => 500,
    num_runs      => 3, num_success_runs => 3, num_failed_runs  => 0,
    num_subs      => 5, num_success_subs => 3, num_failed_subs  => 0,
    num_run_subs  => 3, num_skipped_subs => 2,
);

test_run(
    name          => 'skip in pre_sub',
    runner_args   => {_pre_sub=>sub {
                          my($self, $subname) = @_;
                          if ($subname eq 'Foo::c') {
                              $self->skip('Foo::a');
                              $self->skip(qr/[cb]/);
                          }
                          1;
                      }},
    subs          => ['Foo::a'],
    status        => 200,
    num_runs      => 2, num_success_runs => 2, num_failed_runs  => 0,
    num_subs      => 5, num_success_subs => 2, num_failed_subs  => 0,
    num_run_subs  => 2, num_skipped_subs => 3,
);
test_run(
    name          => 'skip in post_sub',
    runner_args   => {_post_sub=>sub {
                          my($self, $subname) = @_;
                          if ($subname eq 'Foo::c') {
                              $self->skip('Foo::a');
                              $self->skip(qr/[cb]/);
                          }
                          1;
                      }},
    subs          => ['Foo::a'],
    status        => 200,
    num_runs      => 3, num_success_runs => 3, num_failed_runs  => 0,
    num_subs      => 5, num_success_subs => 3, num_failed_subs  => 0,
    num_run_subs  => 3, num_skipped_subs => 2,
);
# XXX test skip inside sub?

test_run(
    name          => 'jump',
    runner_args   => {_post_sub=>sub {
                          my($self, $subname) = @_;
                          if ($subname eq 'Foo::c') {
                              $self->jump('Foo::a');
                          }
                          1;
                      }},
    subs          => ['Foo::a'],
    status        => 200,
    output_re     => qr/^EDCAB$/,
);

test_run(
    name          => 'jump',
    runner_args   => {_pre_sub=>sub {
                          my($self, $subname) = @_;
                          if ($subname eq 'Foo::c') {
                              $self->branch_done('Foo::c', 1);
                          }
                          1;
                      }},
    subs          => ['Foo::a'],
    status        => 200,
    output_re     => qr/^ED$/,
);
# XXX test load_modules=1

done_testing();

sub test_run {
    my (%args) = @_;

    subtest $args{name} => sub {

        my $runner = Sub::Spec::Runner::Orderly->new(
            %{$args{runner_args} // {}});
        $runner->load_modules(0);
        $runner->args($args{args}) if $args{args};

        eval {
            $runner->add($_) for @{ $args{subs} };
        };
        my $eval_err = $@;
        if ($args{add_dies}) {
            ok($eval_err, "add dies");
        }

        if ($args{check_runner_before_run}) {
            ok($args{check_runner_before_run}->($runner),
               "check_runner_before_run");
        }

        my $res;
        my %run_args = (ignore_errors=>$args{ignore_errors});
        if ($args{status}) {
            if (defined($args{output_re})) {
                my ($stdout, $stderr) = capture {
                    $res = $runner->run(%run_args);
                };
                like($stdout // "", $args{output_re}, "output_re")
                    or diag("output is $stdout");
            } else {
                $res = $runner->run(%run_args);
            }

            if ($args{status}) {
                is($res->[0], $args{status}, "return status = $args{status}") or
                    do { diag explain $res; last };
            }
        }

        for (qw(
                   num_success_runs
                   num_failed_runs
                   num_runs
                   num_success_subs
                   num_failed_subs
                   num_subs
                   num_run_subs
                   num_skipped_subs
           )) {
            if (defined $args{$_}) {
                is($res->[2]{$_}, $args{$_}, $_);
            }
        }

        if ($args{check_res}) {
            ok($args{check_res}->($res), "check_res");
        }

        if ($args{more_tests}) {
            $args{more_tests}->($runner);
        }
    };
}

