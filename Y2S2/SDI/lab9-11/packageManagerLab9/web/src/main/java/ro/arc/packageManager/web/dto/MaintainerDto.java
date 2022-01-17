package ro.arc.packageManager.web.dto;

import lombok.*;

@NoArgsConstructor
@AllArgsConstructor
@Data
@EqualsAndHashCode(callSuper = true)
@ToString(callSuper = true)
public class MaintainerDto extends BaseDto{
    private String userName;
    private String fullName;
    private String email;
}
