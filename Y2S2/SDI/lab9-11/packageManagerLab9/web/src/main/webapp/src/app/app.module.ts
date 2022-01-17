import { NgModule } from '@angular/core';
import { BrowserModule } from '@angular/platform-browser';

import { AppRoutingModule } from './app-routing.module';
import { AppComponent } from './app.component';

import { FormsModule } from '@angular/forms';
import { HttpClientModule } from '@angular/common/http';
import { PackagesComponent } from './packages/packages.component';
import { PackageListComponent } from './packages/package-list/package-list.component';
import { PackageService } from './packages/shared/package.service';

@NgModule({
  declarations: [
    AppComponent,
    PackagesComponent,
    PackageListComponent
  ],
  imports: [
    BrowserModule,
    FormsModule,
    HttpClientModule,
    AppRoutingModule,
  ],
  providers: [PackageService],
  bootstrap: [AppComponent]
})
export class AppModule { }
