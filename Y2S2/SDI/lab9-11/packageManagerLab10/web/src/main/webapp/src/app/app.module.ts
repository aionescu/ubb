import { NgModule } from '@angular/core';
import { BrowserModule } from '@angular/platform-browser';

import { AppRoutingModule } from './app-routing.module';
import { AppComponent } from './app.component';

import { FormsModule } from '@angular/forms';
import { HttpClientModule } from '@angular/common/http';
import { PackagesComponent } from './packages/packages.component';
import { PackageListComponent } from './packages/package-list/package-list.component';
import { PackageService } from './packages/shared/package.service';
import { MaintainersComponent } from './maintainers/maintainers.component';
import { MaintainerListComponent } from './maintainers/maintainer-list/maintainer-list.component';
import { MaintainerService } from './maintainers/shared/maintainer.service';
import { AddPackageComponent } from './add-package/add-package.component';
import { AddMaintainerComponent } from './add-maintainer/add-maintainer.component';
import { UpdatePackageComponent } from './update-package/update-package.component';
import { UpdateMaintainerComponent } from './update-maintainer/update-maintainer.component';
import { DeletePackageComponent } from './delete-package/delete-package.component';
import { DeleteMaintainerComponent } from './delete-maintainer/delete-maintainer.component';

@NgModule({
  declarations: [
    AppComponent,
    PackagesComponent,
    PackageListComponent,
    MaintainersComponent,
    MaintainerListComponent,
    AddPackageComponent,
    AddMaintainerComponent,
    UpdatePackageComponent,
    UpdateMaintainerComponent,
    DeletePackageComponent,
    DeleteMaintainerComponent
  ],
  imports: [
    BrowserModule,
    FormsModule,
    HttpClientModule,
    AppRoutingModule,
  ],
  providers: [PackageService, MaintainerService],
  bootstrap: [AppComponent]
})
export class AppModule { }
