module Xmobar.User.Util
    ( rootdir
    , fc
    , fn
    , action
    , icon
    , border
    , wrap
    , withHighArgs
    , withLowArgs
    , sep
    ) where



import System.Environment (getEnv)
import System.IO.Unsafe   (unsafeDupablePerformIO)
import qualified Theme.Palette as XbarTheme



-- rootdir = "\"${XMONAD_CONFIG_DIR}\""
rootdir :: String
rootdir = unsafeDupablePerformIO (getEnv "XMONAD_CONFIG_DIR")

-- | Use xmobar escape codes to output a string with given foreground
--   and background colors.
fc
    :: String  -- ^ foreground color: a color name, or #rrggbb format
    -> String  -- ^ background color
    -> String  -- ^ output string
    -> String
fc fg bg = wrap t "</fc>"
    where t = concat ["<fc=", fg, if null bg then "" else "," ++ bg, ">"]

-- | Use xmobar escape codes to output a string with the font at the given index
fn
    :: Int     -- ^ index: index of the font to use (0: standard font)
    -> String  -- ^ output string
    -> String
fn index = wrap ("<fn=" ++ show index ++ ">") "</fn>"

action
    :: String -- ^ Command. Use of backticks (`) will cause a parse error.
    -> Int    -- ^ Buttons 1-5
    -> String -- ^ Displayed/wrapped text.
    -> String
action command button = wrap l r
  where
    l = "<action=`" ++ command ++ "` button=" ++ show button ++ ">"
    r = "</action>"

icon :: String -> String
icon arg = "<icon=" ++ arg ++ "/>"

-- | Use xmobar box to add a border to an arbitrary string.
border
    :: String -- ^ Border type. Possible values:
              --    VBoth, HBoth, Full, Top, Bottom, Left or Right
    -> String -- ^ color: a color name, or #rrggbb format
    -> Int    -- ^ width in pixels
    -> String -- ^ output string
    -> String
border border color width = wrap prefix "</box>"
  where
    prefix =
        "<box type="
        ++ border
        ++ " width="
        ++ show width
        ++ " color="
        ++ color
        ++ ">"

wrap
    :: String  -- ^ left delimiter
    -> String  -- ^ right delimiter
    -> String  -- ^ output string
    -> String
wrap _ _ "" = ""
wrap l r m  = l ++ m ++ r

withHighArgs :: [String] -> [String] -> [String]
withHighArgs args extras = concat
    [ args
        ++ [ "--low",    XbarTheme.foreground1 XbarTheme.palette
           , "--normal", XbarTheme.blue        XbarTheme.palette
           , "--high",   XbarTheme.red         XbarTheme.palette
           ]
    , ["--"]
    , extras
    ]

withLowArgs :: [String] -> [String] -> [String]
withLowArgs args extras = concat
    [ args
        ++ [ "--low",    XbarTheme.red         XbarTheme.palette
           , "--normal", XbarTheme.blue        XbarTheme.palette
           , "--high",   XbarTheme.foreground1 XbarTheme.palette
           ]
    , ["--"]
    , extras
    ]

sep :: String
sep = fc (XbarTheme.background1 XbarTheme.palette) "" $ fn 1 " │ "
