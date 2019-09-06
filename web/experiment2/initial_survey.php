<?php
$data = $_POST['results'];
$filename = "data/pid.txt";
// get number of participants so far - 1
$number = file_get_contents($filename);
// increase the number of the next participant
$newpid = ((int)$number + 1);
file_put_contents($filename, $newpid);
// save answers to separate file
$filename = "data/demography" . $number . ".txt";
// simple text file, parse later
file_put_contents($filename, $data);
echo (int)$number;
?>
