<?php
  header("Access-Control-Allow-Origin: *");
  header("Access-Control-Allow-Headers: *");
  header("Access-Control-Allow-Methods: *");

  $conn = mysqli_connect("localhost", "root", "", "uri_collection");

  $owner = $_GET["owner"];

  $query = "SELECT * FROM URIs WHERE owner = $owner ORDER BY category";
  $result = mysqli_query($conn, $query);

  if (!$result)
    echo json_encode(array());
  else {
    $rows = array();
    while ($r = mysqli_fetch_assoc($result))
      $rows[] = $r;

    header('Content-type: application/json');
    echo json_encode($rows);
  }
?>
