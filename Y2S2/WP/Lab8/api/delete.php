<?php
  header("Access-Control-Allow-Origin: *");
  header("Access-Control-Allow-Headers: *");
  header("Access-Control-Allow-Methods: *");

  $conn = mysqli_connect("localhost", "root", "", "uri_collection");

  $data = json_decode(file_get_contents("php://input"), true);
  $id = $data["id"];
  $owner = $data["owner"];

  $query = "DELETE FROM URIs WHERE id = $id and owner = $owner";
  $result = mysqli_query($conn, $query);

  if (!$result)
    echo false;
  else
    echo true
?>
