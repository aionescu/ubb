import { Component, OnInit } from "@angular/core";
import { Router } from "@angular/router";
import { PackageModel, emptyPackage } from "../packages/shared/package.model";
import { PackageService } from "../packages/shared/package.service";

@Component({
	selector: "app-add-package",
	templateUrl: "./add-package.component.html",
	styleUrls: ["./add-package.component.css"],
	providers: [PackageService]
})
export class AddPackageComponent implements OnInit {
	package: PackageModel = emptyPackage;

	constructor(private router: Router, private service: PackageService) { }

	ngOnInit(): void { }

	async onAdd() {
		this.service.addPackage(this.package).subscribe(() => this.router.navigate(["../packages"]));
	}
}
