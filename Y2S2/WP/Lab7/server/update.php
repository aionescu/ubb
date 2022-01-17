<?php
  if (!isset($_COOKIE["userID"])) {
    echo "notloggedin";
    die;
  }

  $conn = mysqli_connect("localhost", "root", "", "uri_collection");

  $owner = $_COOKIE["userID"];
  $id = $_POST["id"];
  $uri = $_POST["uri"];
  $description = $_POST["description"];
  $category = $_POST["category"];

  $updates = "";
  $hasUpdates = false;

  if (!empty($uri)) {
    $updates .= "uri = '$uri'";
    $hasUpdates = true;
  }

  if (!empty($description)) {
    if ($hasUpdates)
      $updates .= ", ";

    $updates .= "description = '$description'";
    $hasUpdates = true;
  }

  if (!empty($category)) {
    if ($hasUpdates)
      $updates .= ", ";

    $updates .= "category = '$category'";
    $hasUpdates = true;
  }

  if ($hasUpdates) {
    $query = "UPDATE URIs SET $updates WHERE id = '$id' AND owner = $owner";
    $result = mysqli_query($conn, $query);

    if (!$result)
      echo $conn->error;
    else
      echo "true";
  } else
    echo "true"
?>
