#!/usr/bin/perl 

use warnings;
use strict; 

my @spec = (
	"Idle",
	"Idle",
	"Idle",
	"web_file_server1", 
	"web_file_server2", 
	"web_file_server3", 
	"decision_support_db", 
	"media_streaming", 
	"sql_server_log", 
	"os_paging",
	"web_serber_log", 
	"oltp_db",
	"exchange_server", 
	"workstations", 
	"vod"
);

my @ds = (
	"E1",
	"E2",
	"N1",
	"N2"
);

my $max_aggr=5;
my $number_of_test=100;
#Initial test 
open OUT, ">test" or die $!;
foreach (@ds) {
	print OUT "$_ @spec[3 .. 14]\n";
}
close OUT;

#Initial placement
open OUT, ">init_placement" or die $!;
foreach (@ds) {
	print OUT "$_ ";
	for (my $i=0; $i<$max_aggr; $i++) {
		for (my $k=0; $k<$number_of_test; $k++) {
			my $j = int(rand(15));
			print OUT "$spec[$j] ";
		}
	}
	print OUT "\n";
}
close(OUT);

print "test and init_placement files created!\n";
