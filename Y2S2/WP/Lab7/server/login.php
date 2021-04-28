<?php
  $conn = mysqli_connect("localhost", "root", "", "uri_collection");

  $username = $_POST["username"];
  $password = $_POST["password"];

  $query = "SELECT * FROM Users WHERE username = '$username' AND password = '$password'";
  $result = mysqli_query($conn, $query);

  if (mysqli_num_rows($result) == 0)
    echo "false";
  else {
    $row = mysqli_fetch_array($result);
    setcookie("userID", $row["id"], time() + 86400 * 7, "/");
    echo "true";
  }
?>
