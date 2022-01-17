package ro.klan.conferenceManager.entity;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

@Entity
@Table(name = "Papers")
@Data
@NoArgsConstructor
@AllArgsConstructor
public class Paper {
  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  private String name;
  private String topic;

  @ManyToOne
  @JoinColumn(name = "idAuthor", nullable = false)
  private Author author;
}
