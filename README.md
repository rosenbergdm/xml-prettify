# xml-prettify #

A pretty printer / indentation tool for generic XML.


## Installation ##

There are no prerequisites other than GHC (>=6.10).

1. Using cabal-install

    $ cabal install xml-prettify

2. From source archive

    $ tar xzvf xml-prettify-0.0.1.tar.gz

    $ cd xml-prettify-0.0.1

    $ ghc --make Setup.hs

    $ ./Setup configure

    $ ./Setup build

    $ ./Setup install

## Usage ##

    $ cat ugly.xml
    <Configuration><UPnP><UDN><MediaRenderer>6c36e3a3-xxxx=4e1c-ac5f-506e515f0491</MediaRenderer></UDN><MythFronten><DefaultBackend><DBHostName>localhost</DBHostName><DBUserName>mythtv</DBUserName><DBPassword>xxxxxxx</DBPassword><DBName>mythconverg</DBName><DBPort>0</DBPort></DefaultBackend></MythFrontend></UPnP></Configuration>
    $ xml-prettify ugly.xml
    <Configuration>
      <UPnP>
        <UDN>
          <MediaRenderer>
            6c36e3a3-xxxx=4e1c-ac5f-506e515f0491
          </MediaRenderer>
        </UDN>
        <MythFronten>
          <DefaultBackend>
            <DBHostName>
              localhost
            </DBHostName>
            <DBUserName>
              mythtv
            </DBUserName>
            <DBPassword>
              xxxxxxx
            </DBPassword>
            <DBName>
              mythconverg
            </DBName>
            <DBPort>
              0
            </DBPort>
          </DefaultBackend>
        </MythFrontend>
      </UPnP>
    </Configuration>


## Contact ##

For questions, comments and patches, email me at [dmr@davidrosenberg.me](mailto:dmr@davidrosenberg.me).
