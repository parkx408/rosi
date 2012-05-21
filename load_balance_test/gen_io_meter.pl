#!/usr/bin/perl 

use warnings;
use strict; 

#Read Configuration file 
my $cfg_path = shift @ARGV or die "Specify IO meter experiement configuration file";
my %cfg = ();

my $spec_path = shift @ARGV or die "Specify Test Vector file";

sub read_cfg {
	my $line;

	open (CFG, "<$cfg_path") or die "Can't open $cfg_path";
	while (<CFG>) {
		chomp($_);
		if ($_ =~ m/^#Test Description/) {
			$line = <CFG>;
			chomp($line);
			if ($line) {
				$cfg{ 'Test Description' } = $line;
			}
		}
		elsif ($_ =~ m/^#Test Type/) {
			$line = <CFG>;
			chomp($line);
			if ($line ne "Load Balance") {
				die "Wrong Test Type, Test Type must be Load Balance";
			}
		}
		elsif ($_ =~ m/^#Run Time/) {
			$line = <CFG>;
			chomp($line);
			if ($line) {
				$cfg{ 'Run Time' } = $line;
			}
		}
		elsif ($_ =~ m/^#IOmeter Machine IP/) {
			$line = <CFG>;
			chomp($line);
			if ($line) {
				$cfg{ 'IOmeter' } = $line;
			}
		}
		elsif ($_ =~ m/^#Ramp Up Time/) {
			$line = <CFG>;
			chomp($line);
			if ($line) {
				$cfg{ 'Ramp Up Time' } = $line;
			}
			else {
				$cfg{ 'Ramp Up Time' } = 0;
			}
		}
		elsif ($_ =~ m/^#OIO/) {
			$line = <CFG>;
			chomp($line);
			if ($line) {
				$cfg{ 'OIO' } = $line;
			} else {
				die "Configuration file needs to specify OIO";
			}
		}
		elsif ($_ =~ m/^#Max Aggr/) {
			$line = <CFG>;
			chomp($line);
			if ($line) {
				$cfg{ 'Aggr' } = $line;
			} else {
				die "Configuration file needs to specify maximum number of aggregation";
			}
		}
		elsif ($_ =~ m/^#specs/) {
			$line = <CFG>;
			chomp($line);
			my @specs=();
			while ($line) {
				push @specs, [split(' ', $line)];
				$line = <CFG>;
				if ($line) { 
					chomp($line);
				}
			}
			$cfg{ 'Specs' } = \@specs;
		}
		elsif ($_ =~ m/^#Manager/){
			$line = <CFG>;
			chomp($line);
			my @target=();
			while ($line) {
				push @target, [split(' ', $line)];
				$line = <CFG>;
				if ($line) { 
					chomp($line);
				}
			}
			$cfg{ 'Managers' } = \@target;
		}
	}
	close (CFG);
}
#IOmeter IP address
my $iometer_ip = "127.0.0.1";

#IOMeter version 
my $iom_ver = "2006.07.27";

#IOMeter test setup parameters (default values)
my $test_description = "";
my $run_time = 120;
my $ramp_up_time = 100;
my $number_of_workers_disk = "NUMBER_OF_CPUS";
my $number_of_workers_network = 0;
my $record_results = "ALL";
my $cycling_options = "NORMAL"; 
my $workers_start = 1;
my $workers_step = 1;
my $workers_step_type = "LINEAR";
my $targets_start = 1;
my $targets_step = 1;
my $targets_stop_type = "LINEAR";
my $oio_start = 1;
my $oio_end = 61;
my $oio_step = 10;
my $oio_step_type = "LINEAR";

#IOMeter result display parameters (default values)
my $update_frequency = 0;
my $update_type = "WHOLE_TEST";
my $bar1 = "Total I/Os per Second";
my $bar2 = "Total MBs per Second";
my $bar3 = "Average I/O Response Time (ms)";
my $bar4 = "Maximum I/O Response Time (ms)";
my $bar5 = "% CPU Utilization (total)";
my $bar6 = "Total Error Count";

#IOMeter access specification (default values)
my $specification_name = "";
my $default_assignment = "NONE";
my $access_percentage = 100;
my $size = 4096;
my $read = 100;
my $random = 0;
my $delay = 0;
my $burst_length = 1;
my $align = 0;
my $reply = 0;

#IOMeter manager list (default values)
my $manager_id = 1;
my $manager_name = "local";
my $manager_ip = "127.0.0.1";
my $worker = "Worker";
my $worker_type = "DISK";
my $oio = "1";
my $test_connection = "DISABLED";
my $transaction_per_connection = 1;
my $max_disk_size = 0;
my $starting_disk_sector = 0;
my $target = "";
my $target_type = "DISK";

sub print_version {
	print "$iom_ver\n";
}

sub set_test_setup {
	$iometer_ip = $cfg{ 'IOmeter' };
	$test_description = $cfg{ 'Test Description' };
	$run_time = $cfg{ 'Run Time' };
	$ramp_up_time = $cfg{ 'Ramp Up Time' }; 
}

sub print_test_setup {
	set_test_setup();
	print "'TEST SETUP ====================================================================\n";
	print "'Test Description\n";
	print "  $test_description\n";
	print "'Run Time\n";
	print "' hours      minutes    seconds\n";
	print "  0          0          $run_time\n";
	print "'Ramp Up Time (s)\n";
	print "  $ramp_up_time\n";
	print "'Default Disk Workers to Spawn\n";
	print "  $number_of_workers_disk\n";
	print "'Default Network Workers to Spawn\n";
	print "  $number_of_workers_network\n";
	print "'Record Results\n";
	print "  $record_results\n";
	print "'Queue Depth Cycling\n";
	print "' start       end       step        step type\n";
	print "  $oio_start  $oio_end  $oio_step   $oio_step_type\n";
	print "'Test Type\n";
	print "  $cycling_options\n";
	print "'END test setup\n";
}

sub print_result_display {
	print "'RESULTS DISPLAY ===============================================================\n";
	print "'Update Frequency,Update Type\n";
	print "  $update_frequency,$update_type\n";
	print "'Bar chart 1 statistic\n";
	print "  $bar1\n";
	print "'Bar chart 2 statistic\n";
	print "  $bar2\n";
	print "'Bar chart 3 statistic\n";
	print "  $bar3\n";
	print "'Bar chart 4 statistic\n";
	print "  $bar4\n";
	print "'Bar chart 5 statistic\n";
	print "  $bar5\n";
	print "'Bar chart 6 statistic\n";
	print "  $bar6\n";
	print "'END results display\n";
}

my @specifications;
sub set_auto_access_specification {
	foreach my $random_ (@{$cfg{ 'Random' }}) {
		foreach my $read_ (@{$cfg{ 'Read' }}) {
			foreach my $size_ (@{$cfg{ 'Size' }}) {
				my $specification_name = "$random_% random; $read_% read; ${size_}KB"; 
				$size = $size_*1024;
				$read = $read_;
				$random = $random_;
				push (@specifications, [$specification_name, $random, $read, $size]);
			}
		}
	}
}

sub set_specification {
	push (@specifications, ["Idle", 0, 0, 0]);
	push (@specifications, ["Idle", 0, 0, 0]);
	push (@specifications, ["Idle", 0, 0, 0]);
	push (@specifications, ["Idle", 0, 0, 0]);
	push (@specifications, ["Idle", 0, 0, 0]);
	foreach my $spec_ (@{$cfg{ 'Specs' }}) {
		push (@specifications, $spec_);
	}
}	

sub print_access_specifications {
	set_specification();
	print "'ACCESS SPECIFICATIONS =========================================================\n";
	foreach my $spec (@specifications) {
		print "'Access specification name,default assignment\n";
		print "  @$spec[0],$default_assignment\n";
		print "'size,% of size,% reads,% random,delay,burst,align,reply\n";
		print "  @$spec[3],$access_percentage,@$spec[2],@$spec[1],$delay,$burst_length,$align,$reply\n";
	}
	print "'END access specifications\n";
}

my %ip_list=();
my %worker_list=();
my $aggr=5;
sub set_manager {
	$oio = $cfg{'OIO'};
	$aggr = $cfg{'Aggr'};
	foreach my $manager (@{$cfg{ 'Managers' }}) {
		$ip_list{@$manager[0]}=@$manager[1];
		$worker_list{@$manager[0]}{@$manager[2]} = @$manager[3];
	}
}

my %workload=();	
open WL, "<$spec_path" or die "Can't open spec file $spec_path\n";
while (<WL>) {
	chomp ($_);
	my @val;
	@val =  split(' ', $_);
	my $len = scalar @val - 1;
	my @work = @val[1 .. $len];
	$workload{$val[0]} =  \@work;
}

sub print_manager_list {
	set_manager();
	my $manager_id=0;
	print "'MANAGER LIST ==================================================================\n";
	while (my($manager, $ip) = each(%ip_list)) {
		$manager_name = $manager;
		$manager_ip = $ip;
		$manager_id = $manager_id+1;
		print "'Manager ID, manager name\n";
		print "  $manager_id, $manager_name\n";
		print "'Manager network address\n";
		print "  $manager_ip\n";
		my $worker_id=0;
		while (my ($target, $ds_name) = each (%{$worker_list{$manager_name}})) {
			my $i;
			for ($i=0; $i<$aggr; $i++) {
				$worker_id=$i+1;
				print "'Worker\n";
				print "  $ds_name $worker_id\n";
				print "'Worker type\n";
				print "  $worker_type\n";
				print "'Default target settings for worker\n";
				print "'Number of outstanding IOs,test connection rate,transactions per connection\n";
				print "  $oio,$test_connection,$transaction_per_connection\n";
				print "'Disk maximum size,starting sector\n";
				print "  $max_disk_size,$starting_disk_sector\n";
				print "'End default target settings for worker\n";
				print "'Assigned access specs\n";
				my $j;
				my $rep = scalar (@{$workload{$ds_name}});
				for ($j=$i; $j<$rep; $j=$j+$aggr) {
					print "@{$workload{$ds_name}}[$j]\n";
				}
				print "'End assigned access specs\n";
				print "'Target assignments\n";
				print "'Target\n";
				print "  $target\n";
				print "'Target Type\n";
				print "  $target_type\n";
				print "'End target\n";
				print "'End target assignments\n";
				print "'End worker\n";
			}#End worker definition 
		}
		
		print "'End manager\n";	
	}
	print "'END manager list\n";
}


read_cfg();
print_version();
print_test_setup();
print_result_display();
print_access_specifications();
print_manager_list();
print_version();

