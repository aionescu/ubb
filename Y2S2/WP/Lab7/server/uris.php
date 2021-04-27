<?php
  if (!isset($_COOKIE["userID"])) {
    echo "notloggedin";
    die;
  }

  $conn = mysqli_connect("localhost", "root", "", "uri_collection");

  $owner = $_COOKIE["userID"];

  $query = "SELECT * FROM URIs WHERE owner = $owner ORDER BY category";
  $result = mysqli_query($conn, $query);

  if (!$result)
    echo $conn->error;
  else {
    $rows = array();
    while ($r = mysqli_fetch_assoc($result))
      $rows[] = $r;

    header('Content-type: application/json');
    echo json_encode($rows);
  }
?>
