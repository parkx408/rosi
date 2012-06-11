#!/usr/bin/perl 

use warnings;
use strict; 

my @spec = (
	"Idle",
	"Idle",
	"Idle",
	"Idle",
	"Idle",
	"Idle",
	"Idle",
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
	"web_server_log", 
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
my $number_of_test=1;
#Initial test 
open OUT, ">test" or die $!;
foreach (@ds) {
	print OUT "$_ @spec[10 .. 21]\n";
}
close OUT;

#Initial placement
for (my $j=0; $j<200; $j++) {
open OUT, ">init/init_placement$j" or die $!;
foreach (@ds) {
	print OUT "$_ ";
	for (my $i=0; $i<$max_aggr; $i++) {
		for (my $k=0; $k<$number_of_test; $k++) {
			my $j = int(rand(22));
			print OUT "$spec[$j] ";
		}
	}
	print OUT "\n";
}
close(OUT);
}

print "test and init_placement files created!\n";
