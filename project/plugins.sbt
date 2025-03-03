resolvers += "sbt-idea-repo" at "https://mpeltonen.github.com/maven/"

resolvers += Resolver.url("artifactory", url("https://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases"))(Resolver.ivyStylePatterns)
