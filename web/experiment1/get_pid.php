<?php
$filename = "data/pid.txt";
// get number of participants so far - 1
$number = file_get_contents($filename);
// increase the number of the next participant
$newpid = ((int)$number + 1);
file_put_contents($filename, $newpid);
echo (int)$number;
?>
