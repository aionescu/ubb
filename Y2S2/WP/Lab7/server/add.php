<?php
  if (!isset($_COOKIE["userID"])) {
    echo "notloggedin";
    die;
  }

  $conn = mysqli_connect("localhost", "root", "", "uri_collection");

  $owner = $_COOKIE["userID"];
  $uri = $_POST["uri"];
  $description = $_POST["description"];
  $category = $_POST["category"];

  $query = "INSERT INTO URIs(uri, description, category, owner) VALUES ('$uri', '$description', '$category', $owner)";
  $result = mysqli_query($conn, $query);

  if (!$result)
    echo $conn->error;
  else
    echo "true"
?>
