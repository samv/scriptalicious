#!/usr/bin/perl
#
use Module::Build;
# See perldoc Module::Build for details of how this works

Module::Build->new
    ( module_name     => 'Scriptalicious',
      license => 'perl',
      recommends => {
		     # gah, buggy Module::Build
		     #'Pod::Constants' => 0.15,
		     'Term::ReadKey' => 0,
		     'Text::Wrap' => 0,
		     'Time::HiRes' => 0,
		     'YAML' => 0,
		    },
      build_requires => {
			 'Test::More' => 0,
			},
    )->create_build_script;
