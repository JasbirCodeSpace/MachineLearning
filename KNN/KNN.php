<?php
$data = file_get_contents("https://datahub.io/machine-learning/iris/r/iris.csv");
$rows = explode("\n",$data);
$s = array();
foreach($rows as $row) {
    $s[] = str_getcsv($row);
}
$csvData = array_slice($s,1,100);
preProcessing($csvData);



function preProcessing($csvData){
shuffle($csvData);
$train_data = array_slice($csvData,0,50);
$test_data = array_slice($csvData,50,50);

$train_labels = array();

for ($i = 0; $i < count($train_data); $i++) {
  $train_labels[$i] = $train_data[$i][4];
}

KNN($train_data, $test_data[0], 25);
}

function distance($train,$test) {
  $dist = 0;
  for ($i=0; $i < count($train); $i++) {
    $dist = $dist + pow(($train[$i] - $test[$i]), 2);
  }
  return sqrt($dist);
}

function KNN($train_sample,$test,$N) {

  $dist_array = array();
  for ($i=0; $i < count($train_sample); $i++) {
    $dist_array[$i][1] = distance($train_sample[$i], $test);
    $dist_array[$i][0] = $i;
  }

  $labels = array();
  for ($i = 0; $i < $N; $i++) {
    $labels[$i] = $train_sample[$dist_array[$i][0]][4];

  }

  $arr_freq = array_count_values($labels);
  arsort($arr_freq);
  echo array_keys($arr_freq)[0];

}
 ?>
