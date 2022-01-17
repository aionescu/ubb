package ro.klan.conferenceManager.entity;


import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Date;

import javax.persistence.*;

@Entity
@Table(name = "Conferences")
@Data
@NoArgsConstructor
@AllArgsConstructor

public class Conference {
  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  private String name;
  private Date startDate;
  private Date endDate;

  @ManyToOne
  @JoinColumn(name = "idAdmin", nullable = false)
  private Admin admin;
}
