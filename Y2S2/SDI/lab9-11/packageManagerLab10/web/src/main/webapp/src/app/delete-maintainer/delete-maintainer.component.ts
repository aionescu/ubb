import { Component, OnInit } from "@angular/core";
import { Router } from "@angular/router";
import { MaintainerModel, emptyMaintainer } from "../maintainers/shared/maintainer.model";
import { MaintainerService } from "../maintainers/shared/maintainer.service";

@Component({
	selector: "app-delete-maintainer",
	templateUrl: "./delete-maintainer.component.html",
	styleUrls: ["./delete-maintainer.component.css"],
	providers: [MaintainerService]
})
export class DeleteMaintainerComponent implements OnInit {
	maintainer: MaintainerModel = emptyMaintainer;

	constructor(private router: Router, private service: MaintainerService) { }

	ngOnInit(): void { }

	async onDelete() {
		this.service.deleteMaintainer(this.maintainer).subscribe(() => this.router.navigate(["../maintainers"]));
	}
}
