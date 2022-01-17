package ro.arc.packageManager.web.controller;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import ro.arc.packageManager.core.service.PackageVersionService;
import ro.arc.packageManager.web.converter.PackageVersionConverter;
import ro.arc.packageManager.web.dto.PackageVersionDto;
import ro.arc.packageManager.web.dto.PackageVersionsDto;

@RestController
public class PackageVersionController {
    public static final Logger log= LoggerFactory.getLogger(ro.arc.packageManager.web.controller.PackageVersionController.class);

    @Autowired
    private PackageVersionService packageVersionService;

    @Autowired
    private PackageVersionConverter packageVersionConverter;

    @RequestMapping(value = "/packageVersions", method = RequestMethod.POST)
    PackageVersionDto addPackageVersion(@RequestBody PackageVersionDto packageVersionDto){
        log.trace("addPackageVersion - method entered : packageVersion={}", packageVersionDto);
        var resultModel = packageVersionConverter.convertModelToDto(
                packageVersionService.addPackageVersion(
                        packageVersionConverter.convertDtoToModel(packageVersionDto)));
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
    PackageVersionsDto getAllPackageVersions(){
        log.trace("getAllPackageVersions - method entered");
        var toReturn = new PackageVersionsDto(packageVersionConverter.convertModelsToDtos(packageVersionService.getAllPackageVersions()));
        log.trace("getAllPackageVersions - result={}", toReturn);
        return toReturn;
    }

    @RequestMapping(value = "/packageVersions/filter/{type}_{input}", method = RequestMethod.GET)
    PackageVersionsDto getFilteredPackageVersions(@PathVariable String type, @PathVariable String input){
        log.trace("getFilteredPackageVersions - method entered");
        var toReturn = new PackageVersionsDto(packageVersionConverter.convertModelsToDtos(packageVersionService.getFilteredPackageVersions(type, input)));
        log.trace("getFilteredPackageVersions - result={}", toReturn);
        return toReturn;
    }
}
