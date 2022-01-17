package ro.klan.conferenceManager.entity;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.*;



@Entity
@Table(name = "Chairs")
@Data
@NoArgsConstructor
@AllArgsConstructor

public class Chair {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;
    private String name;
    private String email;

   // @OneToOne(mappedBy = "chairs") map one to one with a conference

}
