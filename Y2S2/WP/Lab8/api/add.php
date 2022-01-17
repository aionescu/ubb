<?php
  header("Access-Control-Allow-Origin: *");
  header("Access-Control-Allow-Headers: *");
  header("Access-Control-Allow-Methods: *");

  $conn = mysqli_connect("localhost", "root", "", "uri_collection");

  $data = json_decode(file_get_contents("php://input"), true);
  $uri = $data["uri"];
  $description = $data["description"];
  $category = $data["category"];
  $owner = $data["owner"];

  $query = "INSERT INTO URIs(uri, description, category, owner) VALUES ('$uri', '$description', '$category', $owner)";
  $result = mysqli_query($conn, $query);

  if (!$result)
    echo "2";
  else
    echo "true"
?>
