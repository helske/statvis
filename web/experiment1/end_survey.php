<?php
$data = $_POST['results'];
$pid = $_POST['pid'];
// save answers to separate file
$filename = "data/subjective" . $pid . ".txt";
// simple text file, parse later
file_put_contents($filename, $data);
?>
