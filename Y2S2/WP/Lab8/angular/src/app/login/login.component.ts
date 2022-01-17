import { Component, OnInit } from "@angular/core";
import { Router } from "@angular/router";
import { emptyUser, User } from "../shared/user.model";
import { UserService } from "../shared/user.service";

@Component({
	selector: "app-login",
	templateUrl: "./login.component.html",
	styleUrls: ["./login.component.css"],
	providers: [UserService]
})
export class LoginComponent implements OnInit {
	user: User = emptyUser;

	constructor(private router: Router, private service: UserService) { }

	ngOnInit(): void { }

	async onLogin() {
		this.service.login(this.user).then(result => {
      if (!result)
        window.alert("Invalid login.")
      else {
        window.alert("You are now logged in.")
        this.router.navigate([".."])
      }
    });
	}
}
