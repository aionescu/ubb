<?php
  header("Access-Control-Allow-Origin: *");
  header("Access-Control-Allow-Headers: *");
  header("Access-Control-Allow-Methods: *");

  $conn = mysqli_connect("localhost", "root", "", "uri_collection");

  $data = json_decode(file_get_contents("php://input"), true);
  $id = $data["id"];
  $uri = $data["uri"];
  $description = $data["description"];
  $category = $data["category"];
  $owner = $data["owner"];

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
      echo false;
    else
      echo true;
  } else
    echo true
?>
