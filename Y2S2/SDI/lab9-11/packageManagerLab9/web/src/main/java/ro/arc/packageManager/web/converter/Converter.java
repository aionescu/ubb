package ro.arc.packageManager.web.converter;


import ro.arc.packageManager.core.domain.BaseEntity;
import ro.arc.packageManager.web.dto.BaseDto;

/**
 * Created by radu.
 */

public interface Converter<Model extends BaseEntity<Long>, Dto extends BaseDto> {

    Model convertDtoToModel(Dto dto);

    Dto convertModelToDto(Model model);

}

