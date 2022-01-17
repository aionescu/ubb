<?php
  if (!isset($_COOKIE["userID"])) {
    echo "notloggedin";
    die;
  }

  $conn = mysqli_connect("localhost", "root", "", "uri_collection");

  $owner = $_COOKIE["userID"];
  $id = $_POST["id"];

  $query = "DELETE FROM URIs WHERE id = $id and owner = $owner";
  $result = mysqli_query($conn, $query);

  if (!$result)
    echo $conn->error;
  else
    echo "true"
?>
