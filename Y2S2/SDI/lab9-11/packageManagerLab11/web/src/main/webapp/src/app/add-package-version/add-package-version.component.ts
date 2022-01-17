import { Component, OnInit } from "@angular/core";
import { Router } from "@angular/router";
import { PackageVersionModel, emptyPackageVersion } from "../package-versions/shared/packageVersion.model";
import { PackageVersionService } from "../package-versions/shared/packageVersion.service";

@Component({
	selector: "app-add-package-version",
	templateUrl: "./add-package-version.component.html",
	styleUrls: ["./add-package-version.component.css"],
	providers: [PackageVersionService]
})
export class AddPackageVersionComponent implements OnInit {
	packageVersion: PackageVersionModel = emptyPackageVersion;

	constructor(private router: Router, private service: PackageVersionService) { }

	ngOnInit(): void { }

	async onAdd() {
		this.service.addPackageVersion(this.packageVersion).subscribe(() => this.router.navigate(["../package-versions"]));
	}
}
