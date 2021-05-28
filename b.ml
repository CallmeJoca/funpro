type  color = W | B (* W: White , B: Black *)
type  image = L of color (* leaf of one  color *)
            | N of image * image * image * image   (* node  with  fourchildren  *)

