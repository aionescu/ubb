package ro.arc.packageManager.web.controller;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import ro.arc.packageManager.core.domain.PackageVersion;
import ro.arc.packageManager.core.service.PackageService;
import ro.arc.packageManager.core.service.PackageVersionService;
import ro.arc.packageManager.web.converter.PackageVersionConverter;
import ro.arc.packageManager.web.dto.PackageVersionDto;
import ro.arc.packageManager.web.dto.PackageVersionsDto;

import java.util.Set;

@RestController
public class PackageVersionController {
    public static final Logger log= LoggerFactory.getLogger(ro.arc.packageManager.web.controller.PackageVersionController.class);

    @Autowired
    private PackageVersionService packageVersionService;

    @Autowired
    private PackageService packageService;

    @Autowired
    private PackageVersionConverter packageVersionConverter;

    @RequestMapping(value = "/packageVersions", method = RequestMethod.POST)
    PackageVersionDto addPackageVersion(@RequestBody PackageVersionDto packageVersionDto){
        log.trace("addPackageVersion - method entered : packageVersion={}", packageVersionDto);
        System.out.println(packageVersionDto);
        var packageVersionModel = packageVersionConverter.convertDtoToModel(packageVersionDto);
        var packageModel = packageService.getOnePackage(packageVersionModel.getAPackage().getId());
        packageVersionModel.setAPackage(packageModel);
        System.out.println("pkg vs model");
        System.out.println(packageVersionModel);
        var resultModel = packageVersionConverter.convertModelToDto(packageVersionService.addPackageVersion(packageVersionModel));
        System.out.println("result model");
        System.out.println(resultModel);
        log.trace("addPackageVersion - result={}", resultModel);
        return resultModel;

    }

    @RequestMapping(value="/packageVersions/{id}", method = RequestMethod.PUT)
    PackageVersionDto updatePackageVersion(@PathVariable Long id, @RequestBody PackageVersionDto packageVersionDto)
    {
        log.trace("updatePackageVersion -method entered : packageVersion={}", packageVersionDto);
        var resultModel = packageVersionConverter.convertModelToDto(
                packageVersionService.updatePackageVersion(
                        packageVersionConverter.convertDtoToModel(packageVersionDto)));
        log.trace("updatePackageVersion - result={}", resultModel);
        return resultModel;

    }

    @RequestMapping(value="/packageVersions/{id}", method = RequestMethod.DELETE)
    ResponseEntity<?> deletePackageVersion(@PathVariable Long id)
    {
        log.trace("deletePackageVersion - method entered : id={}", id);
        packageVersionService.deletePackageVersion(id);
        log.trace("deletePackageVersion - finished");
        return new ResponseEntity<>(HttpStatus.OK);

    }

    @RequestMapping(value="/packageVersions", method = RequestMethod.GET)
    Set<PackageVersionDto> getAllPackageVersions(){
        log.trace("getAllPackageVersions - method entered");
        var toReturn = packageVersionConverter.convertModelsToDtos(packageVersionService.getAllPackageVersions());
        log.trace("getAllPackageVersions - result={}", toReturn);
        return toReturn;
    }

    @RequestMapping(value = "/packageVersions/filter/packageID={packageID}", method = RequestMethod.GET)
    Set<PackageVersionDto> getFilteredPackageVersions(@PathVariable Long packageID){
        log.trace("getFilteredPackageVersions - method entered");
        var aPackage = this.packageService.getOnePackage(packageID);
        var toReturn = packageVersionConverter.convertModelsToDtos(packageVersionService.getFilteredPackageVersions(aPackage));
        System.out.println(toReturn);
        log.trace("getFilteredPackageVersions - result={}", toReturn);
        return toReturn;
    }
}
