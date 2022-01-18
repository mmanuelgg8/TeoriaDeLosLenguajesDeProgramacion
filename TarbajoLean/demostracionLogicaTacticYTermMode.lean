
import init.data.int.basic

variables a b c d : ℤ 

example : a + 0 = a := int.add_zero a
example : 0 + a = a := int.zero_add a
example : a + b = b + a := int.add_comm a b
-- Si en algún momento necesitamos dejar por hacer una demostracion, podemos usar sorry
example : (a + b) * c = a * c + b * c := sorry
-- De esta forma no queda demostrado, pero nos permite seguir realizando la demostracion
-- por otro lado sin que de error. Es equivalente al pass en Python o undefined en Haskell


namespace hidden -- namespace indica un bloque independiente dentro del codigo que 
-- nos permite reusar el nombre de elementos sin que haya error de renombramiento, 
-- en este caso para definir nat nos hace falta porque dicha estructura ya 
-- se encuentra implementada en Lean

inductive nat : Type
| zero : nat
| succ : nat → nat

end hidden
open nat

-- Codigo nuestro
def two_pow : hidden.nat → ℕ 
| hidden.nat.zero        := 1
| (hidden.nat.succ n) := 2 * two_pow n

example (n : hidden.nat) : two_pow hidden.nat.zero = 1 := rfl -- rfl o Eq.refl es el cumplmiento de la propiedad reflexiva
example (n : hidden.nat) : two_pow (hidden.nat.succ n) = 2 * two_pow n := rfl 

def two_powN : ℕ → ℕ
| 0        := 1
| (succ n) := 2 * two_powN n

#eval two_powN 2

example (n : ℕ) : two_powN 0 = 1 := rfl 
example (n : ℕ) : two_powN (n + 1) = 2 * two_powN n := rfl

-- Tactic mode
example (p q r : Prop) : ((p ∨ q) → r) ↔ ((p → r) ∧ (q → r)) :=
begin -- Empezamos el modo tactical
  split, -- Como tenemos que demostrar en ambos sentidos por la doble implicación, usamos split para dividir la demostración en dos
  -- Empezamos a demostrar por la parte izquierda
  { intro h, -- Con 'intro' añadimos una hipótesis asumiendo cierta la premisa de la implicación
    split, -- Como tenemos que de lo anterior se infiere (p → r) ∧ (q → r), volvemos a dividir la demostración
    { intro hp, -- Tenemos que demostrar p → r así que asumimos p cierta
      apply h, -- Aplicando la hipótesis nos queda demostrar que p ∨ q sea cierto
      left, -- Como hemos asumido p cierta aplicamos left que coge el primer constructor de un tipo inductivo cuando hay dos
      assumption}, -- Y aplicamos este tactics que busca en el entorno una hipótesis equivalente a la meta
                   -- Si encuentra una, la usa para demostrar la meta; si no, falla
    { intro hq,
      apply h,
      right,
      assumption}},
  -- Y continuamos por la parte derecha
  { intro h,
    cases h with hpr hqr, -- separamos la hipótesis en dos casos, que se cumpla  p → r o que se cumpla q → r
    intro hpq, -- Y suponemos que p ∨ q es cierto
    cases hpq with hp hq, -- Con ello tenemos otros dos casos: que p sea cierto o que lo sea q
    { apply hpr, -- Aplicamos la hipótesis de que p → r
      assumption},
    { apply hqr,
      assumption}}
end

-- Term mode
example (p q r : Prop) : ((p ∨ q) → r) ↔ ((p → r) ∧ (q → r)) :=
⟨ λ h, ⟨ λ hp, h $ or.inl hp, λ hq, h $ or.inr hq⟩, λ ⟨ hpr, hqr⟩ hpq, hpq.elim hpr hqr⟩ 


