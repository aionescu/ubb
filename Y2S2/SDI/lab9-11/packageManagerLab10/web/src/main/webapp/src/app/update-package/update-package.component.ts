import { Component, OnInit } from "@angular/core";
import { Router } from "@angular/router";
import { PackageModel, emptyPackage } from "../packages/shared/package.model";
import { PackageService } from "../packages/shared/package.service";

@Component({
	selector: "app-update-package",
	templateUrl: "./update-package.component.html",
	styleUrls: ["./update-package.component.css"],
	providers: [PackageService]
})
export class UpdatePackageComponent implements OnInit {
	package: PackageModel = emptyPackage;

	constructor(private router: Router, private service: PackageService) { }

	ngOnInit(): void { }

	async onUpdate() {
		this.service.updatePackage(this.package).subscribe(() => this.router.navigate(["../packages"]));
	}
}
