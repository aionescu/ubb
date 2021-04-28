import { Component, OnInit } from "@angular/core";
import { Router } from "@angular/router";
import { URIService } from "../shared/uri.service";

@Component({
	selector: "app-login",
	templateUrl: "./login.component.html",
	styleUrls: ["./login.component.css"],
	providers: [URIService]
})
export class LoginComponent implements OnInit {
	username = "";
  password = "";

	constructor(private router: Router, private service: URIService) { }

	ngOnInit(): void { }

	async onLogin() {
		this.service.login(this.username, this.password).then(result => {
      if (!result)
        window.alert("Invalid login.")
      else {
        window.alert("You are now logged in.")
        this.router.navigate([".."])
      }
    });
	}
}
