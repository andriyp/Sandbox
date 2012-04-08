{-# LANGUAGE TypeOperators #-}

data A o = o :-> o
         deriving ( Eq, Show, Read )

data C o = ExC [o] [A o] -- extensional
         | InC           -- intensional
         deriving ( Eq, Show, Read )

data F o = C o :~> C o

data NT o = F o :=> F o

-- data Functor c = c :-> c

-- data NatTran c = Functor c :=> Functor c
simpleCat = C [a, b, c, d, e]
              [ a :-> b
              , c, d) ]
  where (a:b:c:d:e:_) = ['a'..]

lol = mapM_ (putStrLn . mkEnterprise)
      [ "SystemFilterCascadingNewFilterPoolReferenceFPMgrAction"
      , "TerminalServiceSubSystemConfigurationAdapterFactory"
      , "RemoteProcessSubSystemConfigurationAdapterFactory"
      , "SystemFilterCascadingNewFilterPoolReferenceAction"
      , "ShellServiceSubSystemConfigurationAdapterFactory"
      , "SystemFilterNewFilterPoolWizardMainPageInterface"
      , "SystemTeamViewSubSystemConfigurationPropertyPage"
      , "SystemFileSubSystemConfigurationAPIProviderImpl"
      , "SystemFilterWorkWithFilterPoolsRefreshAllAction"
      , "RemoteFileSubSystemConfigurationAdapterFactory"
      ]
  where
    mkEnterprise c
      = c ++ " business" ++ c ++ " = new " ++ c ++ "();"
        
