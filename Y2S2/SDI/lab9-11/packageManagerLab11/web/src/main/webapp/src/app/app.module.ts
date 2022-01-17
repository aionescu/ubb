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
import { PackageMaintainersComponent } from './package-maintainers/packageMaintainers.component';
import { PackageMaintainerListComponent } from './package-maintainers/package-maintainer-list/package-maintainer-list.component';
import { PackageMaintainerService } from './package-maintainers/shared/packageMaintainer.service';
import { AddPackageMaintainerComponent } from './add-package-maintainer/add-package-maintainer.component';
import { DeletePackageMaintainerComponent } from './delete-package-maintainer/delete-package-maintainer.component';
import { PackageVersionsComponent } from './package-versions/packageVersions.component';
import { PackageVersionListComponent } from './package-versions/package-version-list/package-version-list.component';
import { PackageVersionService } from './package-versions/shared/packageVersion.service';
import { AddPackageVersionComponent } from './add-package-version/add-package-version.component';
import { DeletePackageVersionComponent } from './delete-package-version/delete-package-version.component';

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
    DeleteMaintainerComponent,
    PackageMaintainersComponent,
    PackageMaintainerListComponent,
    AddPackageMaintainerComponent,
    DeletePackageMaintainerComponent,
    PackageVersionsComponent,
    PackageVersionListComponent,
    AddPackageVersionComponent,
    DeletePackageVersionComponent
  ],
  imports: [
    BrowserModule,
    FormsModule,
    HttpClientModule,
    AppRoutingModule,
  ],
  providers: [PackageService, MaintainerService, PackageMaintainerService, PackageVersionService],
  bootstrap: [AppComponent]
})
export class AppModule { }
