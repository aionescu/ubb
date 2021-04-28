
import { Component, OnInit } from "@angular/core";
import { Router } from "@angular/router";
import { emptyURI, URI } from "../shared/uri.model";
import { URIService } from "../shared/uri.service";

@Component({
	selector: "app-add",
	templateUrl: "./add.component.html",
	styleUrls: ["./add.component.css"],
	providers: [URIService]
})
export class AddComponent implements OnInit {
	uri: URI = emptyURI;

	constructor(private router: Router, private service: URIService) { }

	ngOnInit(): void { }

	async onAdd() {
		this.service.add(this.uri).subscribe(result => {
      if (!result)
        window.alert("Not logged in.")
      else {
        window.alert("URI added successfully.")
        this.router.navigate(["../view"])
      }
    });
	}
}
