
import { Component, OnInit } from "@angular/core";
import { Router } from "@angular/router";
import { emptyURI, URI } from "../shared/uri.model";
import { URIService } from "../shared/uri.service";

@Component({
	selector: "app-delete",
	templateUrl: "./delete.component.html",
	styleUrls: ["./delete.component.css"],
	providers: [URIService]
})
export class DeleteComponent implements OnInit {
	uri: URI = emptyURI;

	constructor(private router: Router, private service: URIService) { }

	ngOnInit(): void { }

	async onDelete() {
		this.service.delete(this.uri).subscribe(result => {
      if (!result)
        window.alert("Not logged in.")
      else {
        window.alert("URI deleted successfully.")
        this.router.navigate(["../view"])
      }
    });
	}
}
