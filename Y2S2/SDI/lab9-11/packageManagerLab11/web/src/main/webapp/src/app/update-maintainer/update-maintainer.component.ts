import { Component, OnInit } from "@angular/core";
import { Router } from "@angular/router";
import { MaintainerModel, emptyMaintainer } from "../maintainers/shared/maintainer.model";
import { MaintainerService } from "../maintainers/shared/maintainer.service";

@Component({
	selector: "app-update-maintainer",
	templateUrl: "./update-maintainer.component.html",
	styleUrls: ["./update-maintainer.component.css"],
	providers: [MaintainerService]
})
export class UpdateMaintainerComponent implements OnInit {
	maintainer: MaintainerModel = emptyMaintainer;

	constructor(private router: Router, private service: MaintainerService) { }

	ngOnInit(): void { }

	async onUpdate() {
		this.service.updateMaintainer(this.maintainer).subscribe(() => this.router.navigate(["../maintainers"]));
	}
}
