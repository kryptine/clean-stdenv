definition module EstherTransform

import EstherBackend

generic transform e :: !e -> Core

derive transform EITHER, CONS, FIELD, OBJECT, Core
derive transform NTexpression