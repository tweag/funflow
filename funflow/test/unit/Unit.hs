-- Funflow unit tests

import Test.QuickCheck (quickCheck)
import Funflow.Tasks.Docker (DockerTaskInput)
import TInstances()

prop_DockerTaskInputMonoidAssociativity :: DockerTaskInput -> DockerTaskInput -> DockerTaskInput -> Bool
prop_DockerTaskInputMonoidAssociativity x y z = (x <> y) <> z == x <> (y <> z)

prop_DockerTaskInputMonoidLeftIdentity :: DockerTaskInput -> Bool
prop_DockerTaskInputMonoidLeftIdentity x = mempty <> x == x

prop_DockerTaskInputMonoidRightIdentity :: DockerTaskInput -> Bool
prop_DockerTaskInputMonoidRightIdentity x = x <> mempty == x

main :: IO ()
main = do
	quickCheck prop_DockerTaskInputMonoidAssociativity
	quickCheck prop_DockerTaskInputMonoidLeftIdentity
	quickCheck prop_DockerTaskInputMonoidRightIdentity
	putStrLn "done!"
