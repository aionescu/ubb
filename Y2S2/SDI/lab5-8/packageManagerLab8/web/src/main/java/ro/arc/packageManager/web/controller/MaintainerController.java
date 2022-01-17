package ro.arc.packageManager.web.controller;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import ro.arc.packageManager.core.domain.exceptions.AppException;
import ro.arc.packageManager.core.service.MaintainerService;
import ro.arc.packageManager.web.converter.MaintainerConverter;
import ro.arc.packageManager.web.dto.MaintainerDto;
import ro.arc.packageManager.web.dto.MaintainersDto;

@RestController
public class MaintainerController {
    public static final Logger log= LoggerFactory.getLogger(MaintainerController.class);

    @Autowired
    private MaintainerService maintainerService;

    @Autowired
    private MaintainerConverter maintainerConverter;

    @RequestMapping(value = "/maintainers", method = RequestMethod.POST)
    MaintainerDto addMaintainer(@RequestBody MaintainerDto maintainerDto){
        log.trace("addMaintainer - method entered : maintainer={}", maintainerDto);
        var resultModel = maintainerConverter.convertModelToDto(
                maintainerService.addMaintainer(
                        maintainerConverter.convertDtoToModel(maintainerDto)));
        log.trace("addMaintainer - result={}", resultModel);
        return resultModel;

    }

    @RequestMapping(value="/maintainers/{id}", method = RequestMethod.PUT)
    MaintainerDto updateMaintainer(@PathVariable Long id, @RequestBody MaintainerDto maintainerDto)
    {
        log.trace("updateMaintainer -method entered : maintainer={}", maintainerDto);
        var resultModel = maintainerConverter.convertModelToDto(
                maintainerService.updateMaintainer(
                        maintainerConverter.convertDtoToModel(maintainerDto)));
        log.trace("updateMaintainer - result={}", resultModel);
        return resultModel;

    }

    @RequestMapping(value="/maintainers/{id}", method = RequestMethod.DELETE)
    ResponseEntity<?> deleteMaintainer(@PathVariable Long id)
    {
        log.trace("deleteMaintainer - method entered : id={}", id);
        maintainerService.deleteMaintainer(id);
        log.trace("deleteMaintainer - finished");
        return new ResponseEntity<>(HttpStatus.OK);

    }

    @RequestMapping(value="/maintainers", method = RequestMethod.GET)
    MaintainersDto getAllMaintainers(){
        log.trace("getAllMaintainers - method entered");
        var toReturn = new MaintainersDto(maintainerConverter.convertModelsToDtos(maintainerService.getAllMaintainers()));
        log.trace("getAllMaintainers - result={}", toReturn);
        return toReturn;
    }

    @RequestMapping(value = "/maintainers/filter/{type}_{input}", method = RequestMethod.GET)
    MaintainersDto getFilteredMaintainers(@PathVariable String type, @PathVariable String input){
        log.trace("getFilteredMaintainers - method entered");
        var toReturn = new MaintainersDto(maintainerConverter.convertModelsToDtos(maintainerService.getFilteredMaintainers(type, input)));
        log.trace("getFilteredMaintainers - result={}", toReturn);
        return toReturn;
    }
}
