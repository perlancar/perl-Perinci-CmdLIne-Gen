package Perinci::CmdLine::Gen;

# DATE
# VERSION

use 5.010001;
use strict;
use warnings;
use Log::Any::IfLOG '$log';

use Data::Dump qw(dump);
use File::Which;
use String::Indent qw(indent);

use Exporter qw(import);
our @EXPORT_OK = qw(
                       gen_perinci_cmdline_script
                       gen_pericmd_script
               );

our %SPEC;

sub _pa {
    state $pa = do {
        require Perinci::Access;
        my $pa = Perinci::Access->new;
        $pa;
    };
    $pa;
}

sub _riap_request {
    my ($action, $url, $extras, $main_args) = @_;

    local $ENV{PERL_LWP_SSL_VERIFY_HOSTNAME} = 0
        unless $main_args->{ssl_verify_hostname};

    _pa()->request($action => $url, %{$extras // {}});
}

$SPEC{gen_pericmd_script} = {
    v => 1.1,
    summary => 'Generate Perinci::CmdLine CLI script',
    args => {

        output_file => {
            summary => 'Path to output file',
            schema => ['str*'],
            default => '-',
            cmdline_aliases => { o=>{} },
            tags => ['category:output'],
            'x.schema.entity' => 'filename',
        },
        overwrite => {
            schema => [bool => default => 0],
            summary => 'Whether to overwrite output if previously exists',
            tags => ['category:output'],
        },

        url => {
            summary => 'URL to function (or package, if you have subcommands)',
            schema => 'riap::url*',
            req => 1,
            pos => 0,
        },
        subcommands => {
            'x.name.is_plural' => 1,
            summary => 'List of subcommand entries, where each entry is "name:url"',
            'summary.alt.plurality.singular' => 'Subcommand name followed by colon and function URL',
            description => <<'_',

Optionally, it can be additionally followed by a summary, so:

    NAME:URL[:SUMMARY]

Example (on CLI):

    --subcommand "delete:/My/App/delete_item:Delete an item"

_
            schema => ['array*', of=>'str*'],
            cmdline_aliases => { s=>{} },
        },
        subcommands_from_package_functions => {
            summary => "Form subcommands from functions under package's URL",
            schema => ['bool', is=>1],
            description => <<'_',

This is an alternative to the `subcommand` option. Instead of specifying each
subcommand's name and URL, you can also specify that subcommand names are from
functions under the package URL in `url`. So for example if `url` is `/My/App/`,
hen all functions under `/My/App` are listed first. If the functions are:

    foo
    bar
    baz_qux

then the subcommands become:

    foo => /My/App/foo
    bar => /My/App/bar
    "baz-qux" => /My/App/baz_qux

_
        },
        default_subcommand => {
            summary => 'Will be passed to Perinci::CmdLine constructor',
            schema => 'str*',
        },
        include_package_functions_match => {
            schema => 're*',
            summary => 'Only include package functions matching this pattern',
            links => [
                'subcommands_from_package_functions',
                'exclude_package_functions_match',
            ],
        },
        exclude_package_functions_match => {
            schema => 're*',
            summary => 'Exclude package functions matching this pattern',
            links => [
                'subcommands_from_package_functions',
                'include_package_functions_match',
            ],
        },
        cmdline => {
            summary => 'Specify module to use',
            schema  => 'str',
            default => 'Perinci::CmdLine::Any',
            'x.schema.entity' => 'modulename',
        },
        prefer_lite => {
            summary => 'Prefer Perinci::CmdLine::Lite backend',
            'summary.alt.bool.not' => 'Prefer Perinci::CmdLine::Classic backend',
            schema  => 'bool',
            default => 1,
        },
        pass_cmdline_object => {
            summary => 'Will be passed to Perinci::CmdLine constructor',
            description => <<'_',

Currently irrelevant when generating with Perinci::CmdLine::Inline.

_
            schema  => 'bool',
        },
        log => {
            summary => 'Will be passed to Perinci::CmdLine constructor',
            schema  => 'bool',
        },
        extra_urls_for_version => {
            summary => 'Will be passed to Perinci::CmdLine constructor',
            schema => ['array*', of=>'str*'],
        },
        default_log_level => {
            schema  => ['str', in=>[qw/trace debug info warn error fatal none/]],
        },
        ssl_verify_hostname => {
            summary => q[If set to 0, will add: $ENV{PERL_LWP_SSL_VERIFY_HOSTNAME} = 0;' to code],
            schema  => 'bool',
            default => 1,
        },
        code_before_instantiate_cmdline => {
            schema => 'str',
        },
        code_after_end => {
            schema => 'str',
        },
        read_config => {
            summary => 'Will be passed to Perinci::CmdLine constructor',
            schema => 'bool',
        },
        config_filename => {
            summary => 'Will be passed to Perinci::CmdLine constructor',
            schema => ['any*', of=>[
                'str*',
                'hash*',
                ['array*', of=>['any*', of=>['str*','hash*']]],
            ]],
        },
        config_dirs => {
            summary => 'Will be passed to Perinci::CmdLine constructor',
            'x.name.is_plural' => 1,
            schema => ['array*', of=>'str*'],
        },
        read_env => {
            summary => 'Will be passed to Perinci::CmdLine constructor',
            schema => 'bool',
        },
        env_name => {
            summary => 'Will be passed to Perinci::CmdLine constructor',
            schema => 'str',
        },
        load_module => {
            summary => 'Load extra modules',
            schema => ['array', of=>'str*'],
            'x.schema.element_entity' => 'modulename',
        },
        allow_prereq => {
            summary => 'Allow script to depend on these modules',
            schema => ['array', of=>'str*'],
            description => <<'_',

Sometimes, as in the case of using `Perinci::CmdLine::Inline`, dependency to
some modules (e.g. non-core XS modules) are prohibited because the goal is to
have a free-standing script. This option allows whitelisting some extra modules.

If you use `Perinci::CmdLine::Inline`, this option will be passed to it.

_
            'x.schema.element_entity' => 'modulename',
        },
        interpreter_path => {
            summary => 'What to put on shebang line',
            schema => 'str',
        },
        script_name => {
            schema => 'str',
        },
        script_summary => {
            schema => 'str',
        },
        script_version => {
            summary => 'Use this for version number instead',
            schema => 'str',
        },
        skip_format => {
            summary => 'Assume that function returns raw text which needs no formatting',
            schema  => 'bool',
        },
        use_utf8 => {
            summary => 'Whether to set utf8 flag on output, will be passed to Perinci::CmdLine constructor',
            schema  => 'bool',
        },
        per_arg_json => {
            summary => 'Will be passed to Perinci::CmdLine constructor',
            schema => ['bool*'],
        },
        per_arg_yaml => {
            summary => 'Will be passed to Perinci::CmdLine constructor',
            'x.name.is_plural' => 1,
            schema => ['bool*'],
        },
    },
};
sub gen_pericmd_script {
    my %args = @_;

    local $Data::Dump::INDENT = "    ";

    # XXX schema
    $args{ssl_verify_hostname} //= 1;

    my $output_file = $args{output_file};

    my $script_name = $args{script_name};
    unless ($script_name) {
        if ($output_file eq '-') {
            $script_name = 'script';
        } else {
            $script_name = $output_file;
            $script_name =~ s!.+[\\/]!!;
        }
    }

    my $cmdline_mod = "Perinci::CmdLine::Any";
    my $cmdline_mod_ver = 0;
    if ($args{cmdline}) {
        my $val = $args{cmdline};
        if ($val eq 'any') {
            $cmdline_mod = "Perinci::CmdLine::Any";
        } elsif ($val eq 'classic') {
            $cmdline_mod = "Perinci::CmdLine::Classic";
        } elsif ($val eq 'lite') {
            $cmdline_mod = "Perinci::CmdLine::Lite";
        } elsif ($val eq 'inline') {
            $cmdline_mod = "Perinci::CmdLine::Inline";
        } else {
            $cmdline_mod = $val;
        }
    }

    my $subcommands;
    if ($args{subcommands} && @{ $args{subcommands} }) {
        $subcommands = {};
        for (@{ $args{subcommands} }) {
            my ($sc_name, $sc_url, $sc_summary) = split /:/, $_, 3;
            $subcommands->{$sc_name} = {
                url => $sc_url,
                summary => $sc_summary,
            };
        }
    } elsif ($args{subcommands_from_package_functions}) {
        my $res = _riap_request(child_metas => $args{url} => {detail=>1}, \%args);
        return [500, "Can't child_metas $args{url}: $res->[0] - $res->[1]"]
            unless $res->[0] == 200;
        $subcommands = {};
        for my $uri (keys %{ $res->[2] }) {
            next unless $uri =~ /\A\w+\z/; # functions only
            my $meta = $res->[2]{$uri};
            if ($args{include_package_functions_match}) {
                next unless $uri =~ /$args{include_package_functions_match}/;
            }
            if ($args{exclude_package_functions_match}) {
                next if $uri =~ /$args{exclude_package_functions_match}/;
            }
            (my $sc_name = $uri) =~ s/_/-/g;
            $subcommands->{$sc_name} = {
                url     => "$args{url}$uri",
                summary => $meta->{summary},
            };
        }
    }

    # request metadata to get summary (etc)
    my $meta;
    {
        my $res = _riap_request(meta => $args{url} => {}, \%args);
        if ($res->[0] == 200) {
            $meta = $res->[2];
        } else {
            warn "Can't meta $args{url}: $res->[0] - $res->[1]"
                if $args{-cmdline};
            $meta = {v=>1.1, _note=>'No meta', args=>{}};
        }
    }

    my $gen_sig = join(
        "",
        "# Note: This script is a CLI ",
        ($meta->{args} ? " for Riap function $args{url}" : ""), # a quick hack to guess meta is func metadata (XXX should've done an info Riap request)
        "\n",
        "# and generated automatically using ", __PACKAGE__,
        " version ", ($Perinci::CmdLine::Gen::VERSION // '?'), "\n",
    );

    my $extra_modules = {};

    # generate code
    my $code;
    if ($cmdline_mod eq 'Perinci::CmdLine::Inline') {
        require Perinci::CmdLine::Inline;
        $cmdline_mod_ver = $Perinci::CmdLine::Inline::VERSION;
        my $res = Perinci::CmdLine::Inline::gen_inline_pericmd_script(
            url => $args{url},
            script_name => $args{script_name},
            script_summary => $args{script_summary},
            script_version => $args{script_version},
            subcommands => $subcommands,
            (default_subcommand => $args{default_subcommand}) x !!$args{default_subcommand},
            log => $args{log},
            (extra_urls_for_version => $args{extra_urls_for_version}) x !!$args{extra_urls_for_version},
            include => $args{load_module},
            code_after_shebang => $gen_sig,
            (code_before_parse_cmdline_options => $args{code_before_instantiate_cmdline}) x !!$args{code_before_instantiate_cmdline},
            (code_after_end => $args{code_after_end}) x !!$args{code_after_end},
            # read_config => $args{read_config}, # currently unsupported
            # config_filename => $args{config_filename}, # currently unsupported
            # config_dirs => $args{config_dirs}, # currently unsupported
            # read_env => $args{read_env}, # currently unsupported
            # env_name => $args{env_name}, # currently unsupported
            shebang => $args{interpreter_path},
            skip_format => $args{skip_format} ? 1:0,
            (use_utf8 => $args{use_utf8} ? 1:0) x !!(defined $args{use_utf8}),
            (allow_prereq => $args{allow_prereq}) x !!$args{allow_prereq},
            (per_arg_json => $args{per_arg_json} ? 1:0) x !!(defined $args{per_arg_json}),
            (per_arg_yaml => $args{per_arg_yaml} ? 1:0) x !!(defined $args{per_arg_yaml}),
        );
        return $res if $res->[0] != 200;
        $code = $res->[2];
        for (keys %{ $res->[3]{'func.raw_result'}{req_modules} }) {
            $extra_modules->{$_} = $res->[3]{'func.raw_result'}{req_modules}{$_};
        }
    } else {
        $extra_modules->{'Log::Any'} = 0 if $args{log};
        # determine minimum required version
        if (defined $args{use_utf8} && $cmdline_mod =~ /\APerinci::CmdLine::(Lite|Any)\z/) {
            if ($cmdline_mod eq 'Perinci::CmdLine::Lite') {
                $cmdline_mod_ver = "1.45";
            } else {
                $extra_modules->{"Perinci::CmdLine::Lite"} = "1.45";
            }
        } elsif ($args{config_filename} && ref($args{config_filename}) eq 'ARRAY' && @{$args{config_filename}} > 1) {
            if ($cmdline_mod eq 'Perinci::CmdLine::Lite') {
                $cmdline_mod_ver = "1.56";
            } else {
                $extra_modules->{'Perinci::CmdLine::Base'} = "1.56";
            }
        }

        $code = join(
            "",
            "#!", ($args{interpreter_path} // $^X), "\n",
            "\n",
            $gen_sig,
            "\n",
            "# DATE\n",
            "# DIST\n",
            "# VERSION\n",
            "\n",
            "use 5.010001;\n",
            "use strict;\n",
            "use warnings;\n",
            ($args{log} ? "use Log::Any;\n" : ""),
            "\n",

            ($args{load_module} && @{$args{load_module}} ?
                 join("", map {"use $_;\n"} @{$args{load_module}})."\n" : ""),

            ($args{default_log_level} ?
                 "BEGIN { no warnings; \$main::Log_Level = '$args{default_log_level}'; }\n\n" : ""),

            "use $cmdline_mod",
            ($cmdline_mod eq 'Perinci::CmdLine::Any' &&
                 defined($args{prefer_lite}) && !$args{prefer_lite} ? " -prefer_lite=>0" : ""),
            ";\n\n",

            ($args{ssl_verify_hostname} ?
                 "" : '$ENV{PERL_LWP_SSL_VERIFY_HOSTNAME} = 0;' . "\n\n"),

            ($args{code_before_instantiate_cmdline} ? "# code_before_instantiate_cmdline\n" . $args{code_before_instantiate_cmdline} . "\n\n" : ""),

            "$cmdline_mod->new(\n",
            "    url => ", dump($args{url}), ",\n",
            (defined($subcommands) ? "    subcommands => " . indent("    ", dump($subcommands), {first_line_indent=>""}) . ",\n" : ""),
            "    program_name => " . dump($script_name) . ",\n",
            (defined($args{default_subcommand}) ? "    default_subcommand => " . dump($args{default_subcommand}) . ",\n" : ""),
            (defined($args{log}) ? "    log => " . dump($args{log}) . ",\n" : ""),
            (defined($args{pass_cmdline_object}) ? "    pass_cmdline_object => " . dump($args{pass_cmdline_object}) . ",\n" : ""),
            (defined($args{extra_urls_for_version}) ? "    extra_urls_for_version => " . dump($args{extra_urls_for_version}) . ",\n" : ""),
            (defined($args{read_config}) ? "    read_config => " . ($args{read_config} ? 1:0) . ",\n" : ""),
            (defined($args{config_filename}) ? "    config_filename => " . dump(ref($args{config_filename}) eq 'ARRAY' && @{$args{config_filename}}==1 ? $args{config_filename}[0] : $args{config_filename}) . ",\n" : ""),
            (defined($args{config_dirs}) ? "    config_dirs => " . dump($args{config_dirs}) . ",\n" : ""),
            (defined($args{read_env})    ? "    read_env => " . ($args{read_env} ? 1:0) . ",\n" : ""),
            (defined($args{env_name})    ? "    env_name => " . dump($args{env_name}) . ",\n" : ""),
            ($args{skip_format} ? "    skip_format => 1,\n" : ""),
            (defined($args{use_utf8}) ? "    use_utf8 => " . dump($args{use_utf8}) . ",\n" : ""),
            (defined($args{per_arg_json}) ? "    per_arg_json => " . dump($args{per_arg_json}) . ",\n" : ""),
            (defined($args{per_arg_yaml}) ? "    per_arg_yaml => " . dump($args{per_arg_yaml}) . ",\n" : ""),
            ")->run;\n",
            "\n",
        );

        # abstract line
        $code .= "# ABSTRACT: " . ($args{script_summary} // $meta->{summary} // $script_name) . "\n";

        # podname
        $code .= "# PODNAME: $script_name\n";

        $code .= "# code_after_end\n" . $args{code_after_end} . "\n\n"
            if $args{code_after_end};

    } # END generate code

    if ($output_file ne '-') {
        $log->tracef("Outputing result to %s ...", $output_file);
        if ((-f $output_file) && !$args{overwrite}) {
            return [409, "Output file '$output_file' already exists (please use --overwrite if you want to override)"];
        }
        open my($fh), ">", $output_file
            or return [500, "Can't open '$output_file' for writing: $!"];

        print $fh $code;
        close $fh
            or return [500, "Can't write '$output_file': $!"];

        chmod 0755, $output_file or do {
            $log->warn("Can't 'chmod 0755, $output_file': $!");
        };

        my $output_name = $output_file;
        $output_name =~ s!.+[\\/]!!;

        if (which("shcompgen") && which($output_name)) {
            $log->trace("We have shcompgen in PATH and output ".
                            "$output_name is also in PATH, running shcompgen ...");
            system "shcompgen", "generate", $output_name;
        }

        $code = "";
    }

    [200, "OK", $code, {
        'func.cmdline_module' => $cmdline_mod,
        'func.cmdline_module_version' => $cmdline_mod_ver,
        'func.cmdline_module_inlined' => ($cmdline_mod eq 'Perinci::CmdLine::Inline' ? 1:0),
        'func.extra_modules' => $extra_modules,
        'func.script_name' => 0,
    }];
}

# alias
{
    no warnings 'once';
    *gen_perinci_cmdline_script = \&gen_pericmd_script;
    $SPEC{gen_perinci_cmdline_script} = $SPEC{gen_pericmd_script};
}

1;
# ABSTRACT:
