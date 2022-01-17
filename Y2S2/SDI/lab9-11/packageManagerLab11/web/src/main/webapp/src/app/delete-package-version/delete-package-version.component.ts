import { Component, OnInit } from "@angular/core";
import { Router } from "@angular/router";
import { PackageVersionModel, emptyPackageVersion } from "../package-versions/shared/packageVersion.model";
import { PackageVersionService } from "../package-versions/shared/packageVersion.service";

@Component({
	selector: "app-delete-package-version",
	templateUrl: "./delete-package-version.component.html",
	styleUrls: ["./delete-package-version.component.css"],
	providers: [PackageVersionService]
})
export class DeletePackageVersionComponent implements OnInit {
	packageVersion: PackageVersionModel = emptyPackageVersion;

	constructor(private router: Router, private service: PackageVersionService) { }

	ngOnInit(): void { }

	async onDelete() {
		this.service.deletePackageVersion(this.packageVersion).subscribe(() => this.router.navigate(["../package-versions"]));
	}
}
