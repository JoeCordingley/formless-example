module Dogs where

import Prelude
import Data.Newtype (class Newtype, unwrap)
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Formless as F
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Data.Array (cons)

type Dog = { name :: String, age :: Age }

newtype Age = Age Int

derive instance newtypeAge :: Newtype Age _

instance showAge :: Show Age where
  show = show <<< unwrap

data AgeError = TooLow | TooHigh | InvalidInt

newtype DogForm (r :: Row Type -> Type) f = DogForm (r
  --          error    input  output
  ( name :: f Void     String String
  , age  :: f AgeError String Age
  ))

derive instance newtypeDogForm :: Newtype (DogForm r f) _

input :: forall m. Monad m => F.Input' DogForm m
input =
  { initialInputs: Just (F.wrapInputFields { name: "name", age: "5" })
  , validators: DogForm
      { name: F.noValidation
      , age: F.hoistFnE_ \str -> case Int.fromString str of
          Nothing -> Left InvalidInt
          Just n
            | n < 0 -> Left TooLow
            | n > 30 -> Left TooHigh
            | otherwise -> Right (Age n)
      }
  }

spec :: forall input m. Monad m => F.Spec' DogForm Dog input m
spec = F.defaultSpec { render = render, handleEvent = F.raiseResult }
  where
  render st@{ form } =
    HH.div_
      [ HH.input
          [ HE.onValueInput $ F.set _name
          ]
      , HH.input
          [ HE.onValueInput $ F.setValidate _age
          ]
      , HH.text case F.getError _age form of
          Nothing -> ""
          Just InvalidInt -> "Age must be an integer"
          Just TooLow -> "Age cannot be negative"
          Just TooHigh -> "No dog has lived past 30 before"
      , HH.button
          [ HE.onClick \_ -> F.submit ]
          [ HH.text "Submit" ]
      ]
    where
    _name = Proxy :: Proxy "name"
    _age = Proxy :: Proxy "age"


data Action = HandleDogForm Dog
data State = Array Dog

page :: forall q i o m. MonadAff m => H.Component q i o m
page = H.mkComponent
  { initialState: const []
  , render: render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }
  where
  handleAction (HandleDogForm dog) = H.modify_ $ cons dog
  render dogs = HH.div_ [ HH.div_  (map renderDog dogs),HH.slot F._formless unit (F.component (const input) spec) unit HandleDogForm]
  renderDog dog = HH.text $ show dog
