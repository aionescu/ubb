
import { Component, OnInit } from "@angular/core";
import { Router } from "@angular/router";
import { emptyURI, URI } from "../shared/uri.model";
import { URIService } from "../shared/uri.service";

@Component({
	selector: "app-update",
	templateUrl: "./update.component.html",
	styleUrls: ["./update.component.css"],
	providers: [URIService]
})
export class UpdateComponent implements OnInit {
	uri: URI = emptyURI;

	constructor(private router: Router, private service: URIService) { }

	ngOnInit(): void { }

	async onUpdate() {
		this.service.update(this.uri).subscribe(result => {
      if (!result)
        window.alert("Not logged in.")
      else {
        window.alert("URI updated successfully.")
        this.router.navigate(["../view"])
      }
    });
	}
}
