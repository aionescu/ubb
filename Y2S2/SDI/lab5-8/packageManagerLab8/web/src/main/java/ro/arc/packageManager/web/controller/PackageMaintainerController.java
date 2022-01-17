package ro.arc.packageManager.web.controller;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import ro.arc.packageManager.core.service.PackageMaintainerService;
import ro.arc.packageManager.web.converter.PackageMaintainerConverter;
import ro.arc.packageManager.web.dto.PackageMaintainerDto;
import ro.arc.packageManager.web.dto.PackageMaintainersDto;

@RestController
public class PackageMaintainerController {
    public static final Logger log= LoggerFactory.getLogger(ro.arc.packageManager.web.controller.PackageMaintainerController.class);

    @Autowired
    private PackageMaintainerService packageMaintainerService;

    @Autowired
    private PackageMaintainerConverter packageMaintainerConverter;

    @RequestMapping(value = "/packageMaintainers", method = RequestMethod.POST)
    PackageMaintainerDto addPackageMaintainer(@RequestBody PackageMaintainerDto packageMaintainerDto){
        log.trace("addPackageMaintainer - method entered : packageMaintainer={}", packageMaintainerDto);
        var resultModel = packageMaintainerConverter.convertModelToDto(
                packageMaintainerService.addPackageMaintainer(
                        packageMaintainerConverter.convertDtoToModel(packageMaintainerDto)));
        log.trace("addPackageMaintainer - result={}", resultModel);
        return resultModel;

    }

    @RequestMapping(value="/packageMaintainers/{id}", method = RequestMethod.PUT)
    PackageMaintainerDto updatePackageMaintainer(@PathVariable Long id, @RequestBody PackageMaintainerDto packageMaintainerDto)
    {
        log.trace("updatePackageMaintainer -method entered : packageMaintainer={}", packageMaintainerDto);
        var resultModel = packageMaintainerConverter.convertModelToDto(
                packageMaintainerService.updatePackageMaintainer(
                        packageMaintainerConverter.convertDtoToModel(packageMaintainerDto)));
        log.trace("updatePackageMaintainer - result={}", resultModel);
        return resultModel;

    }

    @RequestMapping(value="/packageMaintainers/{id}", method = RequestMethod.DELETE)
    ResponseEntity<?> deletePackageMaintainer(@PathVariable Long id)
    {
        log.trace("deletePackageMaintainer - method entered : id={}", id);
        packageMaintainerService.deletePackageMaintainer(id);
        log.trace("deletePackageMaintainer - finished");
        return new ResponseEntity<>(HttpStatus.OK);

    }

    @RequestMapping(value="/packageMaintainers", method = RequestMethod.GET)
    PackageMaintainersDto getAllPackageMaintainers(){
        log.trace("getAllPackageMaintainers - method entered");
        var toReturn = new PackageMaintainersDto(packageMaintainerConverter.convertModelsToDtos(packageMaintainerService.getAllPackageMaintainers()));
        log.trace("getAllPackageMaintainers - result={}", toReturn);
        return toReturn;
    }

    @RequestMapping(value = "/packageMaintainers/filter/{type}_{input}", method = RequestMethod.GET)
    PackageMaintainersDto getFilteredPackageMaintainers(@PathVariable String type, @PathVariable String input){
        log.trace("getFilteredPackageMaintainers - method entered");
        var toReturn = new PackageMaintainersDto(packageMaintainerConverter.convertModelsToDtos(packageMaintainerService.getFilteredPackageMaintainers(type, input)));
        log.trace("getFilteredPackageMaintainers - result={}", toReturn);
        return toReturn;
    }
}
