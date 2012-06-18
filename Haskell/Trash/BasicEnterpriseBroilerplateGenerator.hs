
main = mapM_ (putStrLn . mkEnterprise)
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
        
