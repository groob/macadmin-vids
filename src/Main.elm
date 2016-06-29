module Main exposing (..)

import Html exposing (..)
import String
import Html.App as Html
import Html.Events exposing (onClick, onInput)
import Html.Attributes
  exposing
    ( id
    , attribute
    , property
    , autofocus
    , class
    , href
    , placeholder
    , style
    , value
    , src
    , width
    , height
    )


main : Program Never
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { videos : List Video
  , selected : Video
  , query : String
  , tagFilter : String
  , speakerFilter : String
  }


type alias Video =
  { date : String
  , added : String
  , id : String
  , url : String
  , speakers : List String
  , tags : List String
  , title : String
  }


initialModel : Model
initialModel =
  { videos = videos
  , selected =
      { date = "2014-07-22"
      , added = "2016-06-29"
      , id = "mqK-MAEZekI"
      , url = "https://www.youtube.com/watch?v=mqK-MAEZekI"
      , speakers = [ "Greg Neagle" ]
      , tags = [ "autopkg", "munki", "psu" ]
      , title = "You Oughta Check Out AutoPkg"
      }
  , query = ""
  , tagFilter = ""
  , speakerFilter = ""
  }


init : ( Model, Cmd Msg )
init =
  ( initialModel, Cmd.none )



-- UPDATE


type Filter
  = Tag
  | Speaker


type Msg
  = NoOp
  | Select Video
  | Search String
  | Filter Filter String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      ( model, Cmd.none )

    Select vid ->
      ( { model | selected = vid }, Cmd.none )

    Search query ->
      ( { model | query = query }, Cmd.none )

    Filter filterKind filter ->
      case filterKind of
        Tag ->
          ( { model | tagFilter = filter }, Cmd.none )

        Speaker ->
          ( { model | speakerFilter = filter }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  let
    selected =
      model.selected

    searchedVideos =
      searchFor model.query model.videos

    filteredVideos =
      filterBy model searchedVideos

    footer =
      div
        [ class "page-footer" ]
        [ text "Original idea by "
        , a [ href "https://gophervids.appspot.com" ] [ text "GopherVids" ]
        ]

    selectedID v =
      if v.id == selected.id then
        "selected"
      else
        "unselected"

    videoRow v =
      div
        [ class "video-row"
        , id <| selectedID v
        , onClick <| Select v
        ]
        [ text v.title ]

    videoList =
      List.map (videoRow) filteredVideos

    body =
      let
        speakers x =
          div
            [ class "speaker" ]
            [ div [ onClick <| Filter Speaker x ] [ text x ] ]

        tags x =
          div
            [ class "tag" ]
            [ div [ onClick <| Filter Tag x ] [ text x ] ]

        speakerList =
          List.map speakers selected.speakers

        tagList =
          List.map tags selected.tags

        selectedTag =
          if model.tagFilter /= "" then
            [ text "tag: "
            , text <| model.tagFilter ++ " "
            , button [ onClick <| Filter Tag "" ] [ text "x" ]
            ]
          else
            []

        selectedSpeaker =
          if model.speakerFilter /= "" then
            [ text "speaker: "
            , text <| model.speakerFilter ++ " "
            , button [ onClick <| Filter Speaker "" ] [ text "x" ]
            ]
          else
            []
      in
        div
          [ class "page-body" ]
          [ div
              [ class "video-player" ]
              [ h3 [] [ text selected.title ]
              , youtube selected.id
              , div
                  [ class "speaker-list" ]
                  [ div [ id "title" ] [ text "Speakers:" ]
                  , div [] speakerList
                  ]
              , div
                  [ class "tag-list" ]
                  [ div [ id "title" ] [ text "Tags:" ]
                  , div [] tagList
                  ]
              ]
          , div
              [ class "video-list-container" ]
              [ div
                  [ class "video-selection" ]
                  [ h3 [] [ text "Videos" ]
                  , div [] selectedTag
                  , div [] selectedSpeaker
                  ]
              , div [ class "video-list" ] videoList
              ]
          ]
  in
    div
      [ class "page" ]
      [ header model
      , body
      , footer
      ]


header : Model -> Html Msg
header model =
  div
    [ class "page-header" ]
    [ div
        [ class "header-section" ]
        [ div
            [ class "page-logo" ]
            [ a
                [ href "/" ]
                [ img [ src "/assets/command.jpg" ] []
                , text "MacAdmin Videos"
                ]
            ]
        , div
            [ class "search-box" ]
            [ input [ onInput Search, placeholder "Search" ] []
            ]
        ]
    , div
        [ class "header-section" ]
        [ a
            [ href "https://github.com/groob/elm-videos/issues" ]
            [ Html.i [ class "fa fa-code-fork" ] []
            ]
        ]
    ]


youtube : String -> Html Msg
youtube id =
  div
    [ class "videowrapper" ]
    [ iframe
        [ attribute "allowfullscreen" "true"
        , width 560
        , height 315
        , src <| "https://www.youtube.com/embed/" ++ id
        ]
        []
    ]


searchFor : String -> List Video -> List Video
searchFor query videos =
  let
    queryTerms =
      String.words (String.toLower query)

    matchesQueryTerms { title, speakers, tags } =
      let
        lowerTitle =
          String.toLower title

        lowerSpeakers =
          String.toLower (toString speakers)

        lowerTags =
          String.toLower (toString tags)

        findTerm term =
          String.contains term lowerTitle
            || String.contains term lowerSpeakers
            || String.contains term lowerTags
      in
        List.all findTerm queryTerms
  in
    List.filter matchesQueryTerms videos


filterBy : Model -> List Video -> List Video
filterBy model videos =
  let
    hasTag video =
      (List.member model.tagFilter video.tags) || model.tagFilter == ""

    hasSpeaker video =
      (List.member model.speakerFilter video.speakers) || model.speakerFilter == ""

    bySpeaker =
      List.filter (hasSpeaker) videos

    byTag =
      List.filter (hasTag) bySpeaker
  in
    byTag



-- VIDEO LIST


videos : List Video
videos =
  [ { date = "2014-07-22"
    , added = "2016-06-29"
    , id = "mqK-MAEZekI"
    , url = "https://www.youtube.com/watch?v=mqK-MAEZekI"
    , speakers = [ "Greg Neagle" ]
    , tags = [ "autopkg", "munki", "psu" ]
    , title = "You Oughta Check Out AutoPkg"
    }
  , { date = "2015-08-05"
    , added = "2016-06-29"
    , id = "tcmql5byA_I"
    , url = "https://www.youtube.com/watch?v=tcmql5byA_I"
    , speakers = [ "objc", "programming", "authorization" ]
    , tags = [ "Jeremy Baker", "Tom Burgin" ]
    , title = "Let’s Make Some Decisions: Authorization Plugin Programming"
    }
  , { date = "2015-08-01"
    , added = "2016-06-29"
    , id = "xUrfu9eqlnY"
    , url = "https://www.youtube.com/watch?v=xUrfu9eqlnY"
    , speakers = [ "Edward Marczak", "Russell Hancox" ]
    , tags = [ "psu", "munki", "simian" ]
    , title = "Using Google Open Source Software"
    }
  , { date = "2015-08-13"
    , added = "2016-06-29"
    , id = "kVxnIMirD7c"
    , url = "https://www.youtube.com/watch?v=kVxnIMirD7c"
    , speakers = []
    , tags = []
    , title = "You Suck At Email – And Other Communication Challenges for the IT Pro"
    }
  , { date = "2015-08-13"
    , added = "2016-06-29"
    , id = "vO0Zlfvno3g"
    , url = "https://www.youtube.com/watch?v=vO0Zlfvno3g"
    , speakers = []
    , tags = []
    , title = "DNS - The Inside and Out"
    }
  , { date = "2015-08-13"
    , added = "2016-06-29"
    , id = "b3zfDQ9x-cI"
    , url = "https://www.youtube.com/watch?v=b3zfDQ9x-cI"
    , speakers = []
    , tags = []
    , title = "Connect the Dots with Docker"
    }
  , { date = "2015-08-13"
    , added = "2016-06-29"
    , id = "_8dZhYQd2Ho"
    , url = "https://www.youtube.com/watch?v=_8dZhYQd2Ho"
    , speakers = []
    , tags = []
    , title = "iOS Security Choices via an MDM / EMM"
    }
  , { date = "2015-08-13"
    , added = "2016-06-29"
    , id = "XXmzYY03t64"
    , url = "https://www.youtube.com/watch?v=XXmzYY03t64"
    , speakers = []
    , tags = []
    , title = "The SysAdmin’s Guide to Python"
    }
  , { date = "2015-08-13"
    , added = "2016-06-29"
    , id = "4ZxMNDxI1ig"
    , url = "https://www.youtube.com/watch?v=4ZxMNDxI1ig"
    , speakers = []
    , tags = []
    , title = "From New to New – The Lifecycle of MacBooks"
    }
  , { date = "2015-08-13"
    , added = "2016-06-29"
    , id = "vxuqnVi3170"
    , url = "https://www.youtube.com/watch?v=vxuqnVi3170"
    , speakers = []
    , tags = []
    , title = "Profile Manager, ARD, and Yosemite Server"
    }
  , { date = "2015-08-12"
    , added = "2016-06-29"
    , id = "IhLcwDYESOo"
    , url = "https://www.youtube.com/watch?v=IhLcwDYESOo"
    , speakers = []
    , tags = []
    , title = "To ExtremeZ-IP or not to ExtremeZ-IP:  That is the Question"
    }
  , { date = "2015-08-12"
    , added = "2016-06-29"
    , id = "siq6RroeP70"
    , url = "https://www.youtube.com/watch?v=siq6RroeP70"
    , speakers = []
    , tags = []
    , title = "iOS – Reconnect the Cord"
    }
  , { date = "2015-08-12"
    , added = "2016-06-29"
    , id = "MyvbIXkCZU4"
    , url = "https://www.youtube.com/watch?v=MyvbIXkCZU4"
    , speakers = []
    , tags = []
    , title = "The New Frontier of Apple Deployment and Management"
    }
  , { date = "2015-08-12"
    , added = "2016-06-29"
    , id = "w4WM6M89hmg"
    , url = "https://www.youtube.com/watch?v=w4WM6M89hmg"
    , speakers = []
    , tags = []
    , title = "Using AutoPKG for Windows Software"
    }
  , { date = "2015-08-12"
    , added = "2016-06-29"
    , id = "z53xsrrP_hM"
    , url = "https://www.youtube.com/watch?v=z53xsrrP_hM"
    , speakers = []
    , tags = []
    , title = "Advanced Systems Monitoring with Nagios, PNP and Nconf"
    }
  , { date = "2015-08-11"
    , added = "2016-06-29"
    , id = "q-LHX9Rm-w8"
    , url = "https://www.youtube.com/watch?v=q-LHX9Rm-w8"
    , speakers = []
    , tags = []
    , title = "Load Balancing for Humans 2 - Electric Boogaloo"
    }
  , { date = "2015-08-11"
    , added = "2016-06-29"
    , id = "5TLYbU2C3Xc"
    , url = "https://www.youtube.com/watch?v=5TLYbU2C3Xc"
    , speakers = []
    , tags = []
    , title = "Basic App Development with Swift"
    }
  , { date = "2015-08-11"
    , added = "2016-06-29"
    , id = "qft_tzL2vQc"
    , url = "https://www.youtube.com/watch?v=qft_tzL2vQc"
    , speakers = []
    , tags = []
    , title = "2 years after 1400+ computers in 3 weeks. Are we still nuts?"
    }
  , { date = "2015-08-11"
    , added = "2016-06-29"
    , id = "3R9BJWgfhv8"
    , url = "https://www.youtube.com/watch?v=3R9BJWgfhv8"
    , speakers = []
    , tags = []
    , title = "BONUS! Better Living Through Science Fiction"
    }
  , { date = "2015-08-11"
    , added = "2016-06-29"
    , id = "KV4y-ViVqII"
    , url = "https://www.youtube.com/watch?v=KV4y-ViVqII"
    , speakers = []
    , tags = []
    , title = "DeployStudio - Average Joe to Pro in an Hour"
    }
  , { date = "2015-08-11"
    , added = "2016-06-29"
    , id = "arOO3UUedeA"
    , url = "https://www.youtube.com/watch?v=arOO3UUedeA"
    , speakers = []
    , tags = []
    , title = "OS X System Security at Scale"
    }
  , { date = "2015-08-11"
    , added = "2016-06-29"
    , id = "R6KZgpu4NiY"
    , url = "https://www.youtube.com/watch?v=R6KZgpu4NiY"
    , speakers = []
    , tags = []
    , title = "Virtualization and OS X Testing"
    }
  , { date = "2015-08-11"
    , added = "2016-06-29"
    , id = "X8e2gyS8n_Q"
    , url = "https://www.youtube.com/watch?v=X8e2gyS8n_Q"
    , speakers = []
    , tags = []
    , title = "AD integration and Home Folder Syncing"
    }
  , { date = "2015-08-11"
    , added = "2016-06-29"
    , id = "kRmWO52i-4Q"
    , url = "https://www.youtube.com/watch?v=kRmWO52i-4Q"
    , speakers = []
    , tags = []
    , title = "What's New with Munki"
    }
  , { date = "2015-08-11"
    , added = "2016-06-29"
    , id = "6CY-DCfhAzw"
    , url = "https://www.youtube.com/watch?v=6CY-DCfhAzw"
    , speakers = []
    , tags = []
    , title = "Working with System Frameworks in Python and Objective-C"
    }
  , { date = "2015-08-10"
    , added = "2016-06-29"
    , id = "3Li8-vIRJnw"
    , url = "https://www.youtube.com/watch?v=3Li8-vIRJnw"
    , speakers = []
    , tags = []
    , title = "Virtual Automation - Automated testing with VMware Fusion"
    }
  , { date = "2015-08-10"
    , added = "2016-06-29"
    , id = "AsnDHAnHgYo"
    , url = "https://www.youtube.com/watch?v=AsnDHAnHgYo"
    , speakers = []
    , tags = []
    , title = "Life in a Post-xServe World"
    }
  , { date = "2015-08-10"
    , added = "2016-06-29"
    , id = "cSd82DX-F84"
    , url = "https://www.youtube.com/watch?v=cSd82DX-F84"
    , speakers = []
    , tags = []
    , title = "How to Take a Need and Transform it into a Service"
    }
  , { date = "2015-08-10"
    , added = "2016-06-29"
    , id = "UgmN3b9Vdzg"
    , url = "https://www.youtube.com/watch?v=UgmN3b9Vdzg"
    , speakers = []
    , tags = []
    , title = "Apple Watch: Building Wearable Apps to Boost Student Engagement"
    }
  , { date = "2015-08-07"
    , added = "2016-06-29"
    , id = "iY1y9DQBvVQ"
    , url = "https://www.youtube.com/watch?v=iY1y9DQBvVQ"
    , speakers = []
    , tags = []
    , title = "Entering the Awesome World of SNMP"
    }
  , { date = "2015-08-07"
    , added = "2016-06-29"
    , id = "jgsNxvlfWW8"
    , url = "https://www.youtube.com/watch?v=jgsNxvlfWW8"
    , speakers = []
    , tags = []
    , title = "Take Vacations Using this One Weird Trick - Documentation!"
    }
  , { date = "2015-08-07"
    , added = "2016-06-29"
    , id = "ODsDoYAFjj8"
    , url = "https://www.youtube.com/watch?v=ODsDoYAFjj8"
    , speakers = []
    , tags = []
    , title = "Open (and/or Free) vs Closed Source – Steel Cage Death Match"
    }
  , { date = "2015-08-07"
    , added = "2016-06-29"
    , id = "4-DJU5JILKo"
    , url = "https://www.youtube.com/watch?v=4-DJU5JILKo"
    , speakers = []
    , tags = []
    , title = "The Career Continuum"
    }
  , { date = "2015-08-06"
    , added = "2016-06-29"
    , id = "kiODfRgD4H8"
    , url = "https://www.youtube.com/watch?v=kiODfRgD4H8"
    , speakers = []
    , tags = []
    , title = "vSphere and OS X Server"
    }
  , { date = "2015-08-06"
    , added = "2016-06-29"
    , id = "Jzs2IVk1GOY"
    , url = "https://www.youtube.com/watch?v=Jzs2IVk1GOY"
    , speakers = []
    , tags = []
    , title = "Tips and Tricks for Modern Device Management"
    }
  , { date = "2015-08-06"
    , added = "2016-06-29"
    , id = "NhWIiceWlOA"
    , url = "https://www.youtube.com/watch?v=NhWIiceWlOA"
    , speakers = []
    , tags = []
    , title = "Making the Most out of Apple Configurator"
    }
  , { date = "2015-08-05"
    , added = "2016-06-29"
    , id = "Cmk1Ya1f6MQ"
    , url = "https://www.youtube.com/watch?v=Cmk1Ya1f6MQ"
    , speakers = []
    , tags = []
    , title = "Integrating AutoPkg and Casper with JSSImporter"
    }
  , { date = "2015-08-01"
    , added = "2016-06-29"
    , id = "0WqKrC77eMw"
    , url = "https://www.youtube.com/watch?v=0WqKrC77eMw"
    , speakers = []
    , tags = []
    , title = "Free your NetBoot Server with BSDPy"
    }
  , { date = "2015-08-01"
    , added = "2016-06-29"
    , id = "xSYIgsyueNs"
    , url = "https://www.youtube.com/watch?v=xSYIgsyueNs"
    , speakers = []
    , tags = []
    , title = "It's Dangerous to Go Alone, Take This!"
    }
  , { date = "2015-08-01"
    , added = "2016-06-29"
    , id = "4-EtZizWJdQ"
    , url = "https://www.youtube.com/watch?v=4-EtZizWJdQ"
    , speakers = []
    , tags = []
    , title = "Administering Office 2016 for Mac"
    }
  , { date = "2015-08-01"
    , added = "2016-06-29"
    , id = "UFpOFx9yWPo"
    , url = "https://www.youtube.com/watch?v=UFpOFx9yWPo"
    , speakers = []
    , tags = []
    , title = "To 12,000 Macs and Beyond"
    }
  , { date = "2014-10-31"
    , added = "2016-06-29"
    , id = "l0sv9vgYHD0"
    , url = "https://www.youtube.com/watch?v=l0sv9vgYHD0"
    , speakers = []
    , tags = []
    , title = "Proper Care and Feeding of your IT Professional"
    }
  , { date = "2014-07-22"
    , added = "2016-06-29"
    , id = "tl4iwsBbhWI"
    , url = "https://www.youtube.com/watch?v=tl4iwsBbhWI"
    , speakers = []
    , tags = []
    , title = "Thin-Imaging Macs Using IBM Endpoint Manager"
    }
  , { date = "2014-07-22"
    , added = "2016-06-29"
    , id = "UG84nedo4ag"
    , url = "https://www.youtube.com/watch?v=UG84nedo4ag"
    , speakers = []
    , tags = []
    , title = "Going MAD - Munki, AutoPkg and DeployStudio"
    }
  , { date = "2014-07-22"
    , added = "2016-06-29"
    , id = "mwROh3Ylz_M"
    , url = "https://www.youtube.com/watch?v=mwROh3Ylz_M"
    , speakers = []
    , tags = []
    , title = "Extending OS X Management Systems with Scripting"
    }
  , { date = "2014-07-22"
    , added = "2016-06-29"
    , id = "pMjbCgkc2Aw"
    , url = "https://www.youtube.com/watch?v=pMjbCgkc2Aw"
    , speakers = []
    , tags = []
    , title = "Embracing Mobile Devices and BYOD using MDM"
    }
  , { date = "2014-07-22"
    , added = "2016-06-29"
    , id = "Cbo92MphkEg"
    , url = "https://www.youtube.com/watch?v=Cbo92MphkEg"
    , speakers = []
    , tags = []
    , title = "Citizens of the World - Macs in Windows Server Environments"
    }
  , { date = "2014-07-22"
    , added = "2016-06-29"
    , id = "uAo3PLzX_LY"
    , url = "https://www.youtube.com/watch?v=uAo3PLzX_LY"
    , speakers = []
    , tags = []
    , title = "Advancing from the Help Desk to Admin"
    }
  , { date = "2014-07-22"
    , added = "2016-06-29"
    , id = "sjbESCx-G48"
    , url = "https://www.youtube.com/watch?v=sjbESCx-G48"
    , speakers = []
    , tags = []
    , title = "Automate Yourself Out of a Job"
    }
  , { date = "2014-07-22"
    , added = "2016-06-29"
    , id = "Hf0bt7hk5hY"
    , url = "https://www.youtube.com/watch?v=Hf0bt7hk5hY"
    , speakers = []
    , tags = []
    , title = "Baremetal VSphere Mac Minis"
    }
  , { date = "2014-07-22"
    , added = "2016-06-29"
    , id = "33VnPLNrNYo"
    , url = "https://www.youtube.com/watch?v=33VnPLNrNYo"
    , speakers = []
    , tags = []
    , title = "Hey, Where's my Mac?"
    }
  , { date = "2014-07-22"
    , added = "2016-06-29"
    , id = "Qovhu2W_SuA"
    , url = "https://www.youtube.com/watch?v=Qovhu2W_SuA"
    , speakers = []
    , tags = []
    , title = "Performance Automation with Munki"
    }
  , { date = "2014-07-22"
    , added = "2016-06-29"
    , id = "pcS62-AHSVo"
    , url = "https://www.youtube.com/watch?v=pcS62-AHSVo"
    , speakers = []
    , tags = []
    , title = "SNMP Monitoring - The Sequel"
    }
  , { date = "2014-07-22"
    , added = "2016-06-29"
    , id = "hciIJI3aJuU"
    , url = "https://www.youtube.com/watch?v=hciIJI3aJuU"
    , speakers = []
    , tags = []
    , title = "Shipping Containers with Docker"
    }
  , { date = "2014-07-22"
    , added = "2016-06-29"
    , id = "2UsJev2nGjY"
    , url = "https://www.youtube.com/watch?v=2UsJev2nGjY"
    , speakers = []
    , tags = []
    , title = "Shell Building Blocks"
    }
  , { date = "2014-07-22"
    , added = "2016-06-29"
    , id = "lXudLvptY5w"
    , url = "https://www.youtube.com/watch?v=lXudLvptY5w"
    , speakers = []
    , tags = []
    , title = "Git for System Admins"
    }
  , { date = "2014-07-22"
    , added = "2016-06-29"
    , id = "ob0kH43YITg"
    , url = "https://www.youtube.com/watch?v=ob0kH43YITg"
    , speakers = []
    , tags = []
    , title = "Teaching 1:1 Responsibility"
    }
  , { date = "2014-07-22"
    , added = "2016-06-29"
    , id = "GuVAmUDI6dI"
    , url = "https://www.youtube.com/watch?v=GuVAmUDI6dI"
    , speakers = []
    , tags = []
    , title = "Surviving as a Solo Sysadmin"
    }
  , { date = "2014-07-22"
    , added = "2016-06-29"
    , id = "jLsAA6yjauI"
    , url = "https://www.youtube.com/watch?v=jLsAA6yjauI"
    , speakers = []
    , tags = []
    , title = "Mac Administration - The Fundamentals"
    }
  , { date = "2014-07-22"
    , added = "2016-06-29"
    , id = "Mtz7ZoKIQCE"
    , url = "https://www.youtube.com/watch?v=Mtz7ZoKIQCE"
    , speakers = []
    , tags = []
    , title = "Beyond Thunderdome - Managing OS X w/o OS X"
    }
  , { date = "2014-07-22"
    , added = "2016-06-29"
    , id = "XzG_0zzjWRs"
    , url = "https://www.youtube.com/watch?v=XzG_0zzjWRs"
    , speakers = []
    , tags = []
    , title = "Systems Deployment with Blast Image Config"
    }
  , { date = "2014-07-22"
    , added = "2016-06-29"
    , id = "PrSXU1v10KI"
    , url = "https://www.youtube.com/watch?v=PrSXU1v10KI"
    , speakers = []
    , tags = []
    , title = "SUStenance via Reposado (and Friends)"
    }
  , { date = "2014-07-22"
    , added = "2016-06-29"
    , id = "DjI-DFEjko4"
    , url = "https://www.youtube.com/watch?v=DjI-DFEjko4"
    , speakers = []
    , tags = []
    , title = "Over The Hill or Just Over Tech - Staying Viable in IT"
    }
  , { date = "2014-07-22"
    , added = "2016-06-29"
    , id = "VQXhTPsUlzI"
    , url = "https://www.youtube.com/watch?v=VQXhTPsUlzI"
    , speakers = []
    , tags = []
    , title = "Modular Image Creation"
    }
  , { date = "2014-07-22"
    , added = "2016-06-29"
    , id = "L_9h3_MBEF8"
    , url = "https://www.youtube.com/watch?v=L_9h3_MBEF8"
    , speakers = []
    , tags = []
    , title = "Migrating from PC to Mac - Not as Scary as the Users Think"
    }
  , { date = "2014-07-22"
    , added = "2016-06-29"
    , id = "uLVUGPD8n_A"
    , url = "https://www.youtube.com/watch?v=uLVUGPD8n_A"
    , speakers = []
    , tags = []
    , title = "Where MDM on iOS Leaves Off"
    }
  , { date = "2014-07-22"
    , added = "2016-06-29"
    , id = "BPTJnz27T44"
    , url = "https://www.youtube.com/watch?v=BPTJnz27T44"
    , speakers = []
    , tags = []
    , title = "Multi Tenanted Munki with Puppet and Sal"
    }
  , { date = "2014-07-22"
    , added = "2016-06-29"
    , id = "O97iI3kLEbc"
    , url = "https://www.youtube.com/watch?v=O97iI3kLEbc"
    , speakers = []
    , tags = []
    , title = "What the Heck is Wrong with my Server"
    }
  , { date = "2014-07-22"
    , added = "2016-06-29"
    , id = "9JJuwEmIPIY"
    , url = "https://www.youtube.com/watch?v=9JJuwEmIPIY"
    , speakers = []
    , tags = []
    , title = "Replacing Dual Boots With Virtual Desktops in Managed Macs"
    }
  , { date = "2014-07-22"
    , added = "2016-06-29"
    , id = "YIuvedgEz7k"
    , url = "https://www.youtube.com/watch?v=YIuvedgEz7k"
    , speakers = []
    , tags = []
    , title = "Scripting For Better Package Deployment - Part 2"
    }
  , { date = "2014-07-22"
    , added = "2016-06-29"
    , id = "-aoEwniawjk"
    , url = "https://www.youtube.com/watch?v=-aoEwniawjk"
    , speakers = []
    , tags = []
    , title = "iBeacon in Education"
    }
  , { date = "2014-07-22"
    , added = "2016-06-29"
    , id = "mqK-MAEZekI"
    , url = "https://www.youtube.com/watch?v=mqK-MAEZekI"
    , speakers = []
    , tags = []
    , title = "You Oughta Check Out AutoPkg"
    }
  , { date = "2014-07-22"
    , added = "2016-06-29"
    , id = "WC6oORjzZDs"
    , url = "https://www.youtube.com/watch?v=WC6oORjzZDs"
    , speakers = []
    , tags = []
    , title = "Load Balancing for Humans"
    }
  , { date = "2014-07-22"
    , added = "2016-06-29"
    , id = "_nz-2KirK58"
    , url = "https://www.youtube.com/watch?v=_nz-2KirK58"
    , speakers = []
    , tags = []
    , title = "Managing Mavericks' FileVault 2 with fdesetup"
    }
  , { date = "2014-07-22"
    , added = "2016-06-29"
    , id = "l7dGaULnY4o"
    , url = "https://www.youtube.com/watch?v=l7dGaULnY4o"
    , speakers = []
    , tags = []
    , title = "Manage Third Party Applications with Profiles"
    }
  , { date = "2014-07-22"
    , added = "2016-06-29"
    , id = "Yb2waalErms"
    , url = "https://www.youtube.com/watch?v=Yb2waalErms"
    , speakers = []
    , tags = []
    , title = "Mirror - Leveraging AirPlay Across Enterprise Networks"
    }
  , { date = "2014-07-22"
    , added = "2016-06-29"
    , id = "ku4neL36TSE"
    , url = "https://www.youtube.com/watch?v=ku4neL36TSE"
    , speakers = []
    , tags = []
    , title = "Scripting For Better Package Deployment - Part 1"
    }
  , { date = "2014-07-22"
    , added = "2016-06-29"
    , id = "KXuU-IbEzyg"
    , url = "https://www.youtube.com/watch?v=KXuU-IbEzyg"
    , speakers = []
    , tags = []
    , title = "The One Button Studio and Sandboxing"
    }
  , { date = "2014-07-22"
    , added = "2016-06-29"
    , id = "6lVSIxRgy5A"
    , url = "https://www.youtube.com/watch?v=6lVSIxRgy5A"
    , speakers = []
    , tags = []
    , title = "Vagrant Up!"
    }
  , { date = "2014-07-22"
    , added = "2016-06-29"
    , id = "eSLFB5rxSco"
    , url = "https://www.youtube.com/watch?v=eSLFB5rxSco"
    , speakers = []
    , tags = []
    , title = "UNIX:  Working the Command Line in OS X"
    }
  , { date = "2014-07-22"
    , added = "2016-06-29"
    , id = "W1wkjDuwcL0"
    , url = "https://www.youtube.com/watch?v=W1wkjDuwcL0"
    , speakers = []
    , tags = []
    , title = "Packet Sniffing For Admins"
    }
  ]
