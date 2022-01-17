
import { Component, ElementRef, OnInit, ViewChild } from "@angular/core";
import { Router } from "@angular/router";
import { emptyURI, URI } from "../shared/uri.model";
import { URIService } from "../shared/uri.service";

@Component({
	selector: "app-view",
	templateUrl: "./view.component.html",
	styleUrls: ["./view.component.css"],
	providers: [URIService]
})
export class ViewComponent implements OnInit {
	uris: URI[] = [];
  currentPage: URI[] = [];
  page = 0;
  maxPage = 0;

	constructor(private router: Router, private service: URIService) { }

  private updateCurrentPage() {
    this.currentPage = this.uris.slice(this.page * 4, (this.page + 1) * 4)
  }

	ngOnInit(): void {
    this.service.uris().subscribe(uris => {
      this.uris = uris
      this.page = 0
      this.maxPage = Math.ceil(uris.length / 4) - 1
      this.updateCurrentPage()
    })
  }

  prevPage() {
    if (this.page > 0) {
      --this.page
      this.updateCurrentPage()
    }
  }

  nextPage() {
    if (this.page < this.maxPage) {
      ++this.page
      this.updateCurrentPage()
    }
  }
}
