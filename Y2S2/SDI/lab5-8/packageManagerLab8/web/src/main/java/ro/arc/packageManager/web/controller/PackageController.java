package ro.arc.packageManager.web.controller;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import ro.arc.packageManager.core.service.PackageService;
import ro.arc.packageManager.web.converter.PackageConverter;
import ro.arc.packageManager.web.dto.PackageDto;
import ro.arc.packageManager.web.dto.PackagesDto;

@RestController
public class PackageController {
    public static final Logger log= LoggerFactory.getLogger(ro.arc.packageManager.web.controller.PackageController.class);

    @Autowired
    private PackageService packageService;

    @Autowired
    private PackageConverter packageConverter;

    @RequestMapping(value = "/packages", method = RequestMethod.POST)
    PackageDto addPackage(@RequestBody PackageDto packageDto){
        log.trace("addPackage - method entered : package={}", packageDto);
        var resultModel = packageConverter.convertModelToDto(
                packageService.addPackage(
                        packageConverter.convertDtoToModel(packageDto)));
        log.trace("addPackage - result={}", resultModel);
        return resultModel;

    }

    @RequestMapping(value="/packages/{id}", method = RequestMethod.PUT)
    PackageDto updatePackage(@PathVariable Long id, @RequestBody PackageDto packageDto)
    {
        log.trace("updatePackage -method entered : package={}", packageDto);
        var resultModel = packageConverter.convertModelToDto(
                packageService.updatePackage(
                        packageConverter.convertDtoToModel(packageDto)));
        log.trace("updatePackage - result={}", resultModel);
        return resultModel;

    }

    @RequestMapping(value="/packages/{id}", method = RequestMethod.DELETE)
    ResponseEntity<?> deletePackage(@PathVariable Long id)
    {
        log.trace("deletePackage - method entered : id={}", id);
        packageService.deletePackage(id);
        log.trace("deletePackage - finished");
        return new ResponseEntity<>(HttpStatus.OK);

    }

    @RequestMapping(value="/packages", method = RequestMethod.GET)
    PackagesDto getAllPackages(){
        log.trace("getAllPackages - method entered");
        var toReturn = new PackagesDto(packageConverter.convertModelsToDtos(packageService.getAllPackages()));
        log.trace("getAllPackages - result={}", toReturn);
        return toReturn;
    }

    @RequestMapping(value = "/packages/filter/{type}_{input}", method = RequestMethod.GET)
    PackagesDto getFilteredPackages(@PathVariable String type, @PathVariable String input){
        log.trace("getFilteredPackages - method entered");
        var toReturn = new PackagesDto(packageConverter.convertModelsToDtos(packageService.getFilteredPackages(type, input)));
        log.trace("getFilteredPackages - result={}", toReturn);
        return toReturn;
    }
}
