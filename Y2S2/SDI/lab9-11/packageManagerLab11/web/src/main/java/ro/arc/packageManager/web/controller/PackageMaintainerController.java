package ro.arc.packageManager.web.controller;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import ro.arc.packageManager.core.domain.exceptions.AppException;
import ro.arc.packageManager.core.service.MaintainerService;
import ro.arc.packageManager.core.service.PackageService;
import ro.arc.packageManager.web.converter.PackageMaintainerConverter;
import ro.arc.packageManager.web.dto.PackageMaintainerDto;
import ro.arc.packageManager.web.dto.PackageMaintainersDto;

import java.util.Set;

@RestController
public class PackageMaintainerController {
    public static final Logger log= LoggerFactory.getLogger(ro.arc.packageManager.web.controller.PackageMaintainerController.class);

    @Autowired
    private MaintainerService maintainerService;

    @Autowired
    private PackageService packageService;

    @Autowired
    private PackageMaintainerConverter packageMaintainerConverter;

    @RequestMapping(value = "/packageMaintainers/maintainerID={maintainerID}", method = RequestMethod.POST)
    PackageMaintainerDto addPackageMaintainer(@PathVariable Long maintainerID,
                                              @RequestBody PackageMaintainerDto packageMaintainerDto){
        log.trace("addPackageMaintainer - method entered : packageMaintainer={}", packageMaintainerDto);
        var maintainer = maintainerService.getOneMaintainer(maintainerID);
        var aPackage = packageService.getPackageByName(packageMaintainerDto.getPackageName());
        var importance = packageMaintainerDto.getImportance();
        maintainer.addImportance(aPackage, importance);
        aPackage.addImportance(maintainer, importance);
        maintainerService.addMaintainerAnyCase(maintainer);
        packageService.addPackageAnyCase(aPackage);
        System.out.println(maintainer.getPackageMaintainers());
        PackageMaintainerDto resultModel = PackageMaintainerDto.builder()
                .packageID(aPackage.getId())
                .packageName(aPackage.getName())
                .maintainerID(maintainerID)
                .importance(importance)
                .build();
        log.trace("addPackageMaintainer - exit");
        return resultModel;

    }

    @RequestMapping(value="/packageMaintainers/maintainerID={maintainerID}&packageID={packageID}", method = RequestMethod.DELETE)
    ResponseEntity<?> deletePackageMaintainer(@PathVariable Long maintainerID, @PathVariable Long packageID)
    {
        log.trace("deletePackageMaintainer - method entered : maintainerID={}, packageID={}", maintainerID, packageID);
        var maintainer = this.maintainerService.getOneMaintainer(maintainerID);
        maintainer.deletePackage(packageID);
        log.trace("deletePackageMaintainer - finished");
        return new ResponseEntity<>(HttpStatus.OK);

    }

    @RequestMapping(value="/packageMaintainers/maintainerID={maintainerID}", method = RequestMethod.GET)
    Set<PackageMaintainerDto> getAllPackageMaintainers(@PathVariable Long maintainerID){
        log.trace("getAllPackageMaintainers - method entered");
        var maintainer = maintainerService.getOneMaintainer(maintainerID);
        var toReturn = packageMaintainerConverter.convertModelsToDtos(maintainer.getPackageMaintainers());
        log.trace("getAllPackageMaintainers - result={}", toReturn);
        return toReturn;
    }
}
