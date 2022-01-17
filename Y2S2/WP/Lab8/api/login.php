<?php
  header("Access-Control-Allow-Origin: *");
  header("Access-Control-Allow-Headers: *");
  header("Access-Control-Allow-Methods: *");

  $conn = mysqli_connect("localhost", "root", "", "uri_collection");

  $data = json_decode(file_get_contents("php://input"), true);
  $username = $data["username"];
  $password = $data["password"];

  $query = "SELECT * FROM Users WHERE username = '$username' AND password = '$password'";
  $result = mysqli_query($conn, $query);

  if (mysqli_num_rows($result) == 0)
    echo -1;
  else {
    $row = mysqli_fetch_array($result);
    echo $row["id"];
  }
?>
