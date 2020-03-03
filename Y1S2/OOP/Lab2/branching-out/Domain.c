#include <stdio.h>
#include <stdlib.h>
#include "Utils.h"
#include "Domain.h"

Str stateToStr(State state) {
  static char powder[] = "Powder";
  static char solid[] = "Solid";
  static char viscous[] = "Viscous";
  static char liquid[] = "Liquid";

  switch (state) {
    case Powder:
      return powder;
    case Solid:
      return solid;
    case Viscous:
      return viscous;
    case Liquid:
      return liquid;
    default:
      failWith("Unrecognized ingredient state");
  }
}

Str useToStr(Use use) {
  static char medicinal[] = "Medicinal";
  static char alchemical[] = "Alchemical";
  static char practical[] = "Practical";

  switch (use) {
    case Medicinal:
      return medicinal;
    case Alchemical:
      return alchemical;
    case Practical:
      return practical;
    default:
      failWith("Unrecognized intended use");
  }
}

String ingredientToString(Ingredient ingredient) {
  char* buffer = malloc(100);

  sprintf(buffer,
    "Ingredient #%d, State: %s, Intended use: %s, Potency: %d",
    ingredient.id,
    stateToStr(ingredient.state),
    useToStr(ingredient.intendedUse),
    ingredient.potency);

  return buffer;
}
