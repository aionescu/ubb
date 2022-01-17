import { Component, OnInit } from "@angular/core";
import { Router } from "@angular/router";
import { PackageModel, emptyPackage } from "../packages/shared/package.model";
import { PackageService } from "../packages/shared/package.service";

@Component({
	selector: "app-delete-package",
	templateUrl: "./delete-package.component.html",
	styleUrls: ["./delete-package.component.css"],
	providers: [PackageService]
})
export class DeletePackageComponent implements OnInit {
	package: PackageModel = emptyPackage;

	constructor(private router: Router, private service: PackageService) { }

	ngOnInit(): void { }

	async onDelete() {
		this.service.deletePackage(this.package).subscribe(() => this.router.navigate(["../packages"]));
	}
}
