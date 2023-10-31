module Main where

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

freeTree :: Tree Char
freeTree =
  Node
    'P'
    ( Node
        'O'
        ( Node
            'L'
            (Node 'N' Empty Empty)
            (Node 'T' Empty Empty)
        )
        ( Node
            'Y'
            (Node 'S' Empty Empty)
            (Node 'A' Empty Empty)
        )
    )
    ( Node
        'L'
        ( Node
            'W'
            (Node 'C' Empty Empty)
            (Node 'R' Empty Empty)
        )
        ( Node
            'A'
            (Node 'A' Empty Empty)
            (Node 'C' Empty Empty)
        )
    )

data Direction = L | R deriving (Show)

x -: f = f x

data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)

type Breadcrumbs a = [Crumb a]

type Zipper a = (Tree a, Breadcrumbs a)

goLeft :: Zipper a -> Maybe (Zipper a)
goLeft (Node x l r, bs) = Just (l, LeftCrumb x r : bs)
goLeft (Empty, _) = Nothing

goRight :: Zipper a -> Maybe (Zipper a)
goRight (Node x l r, bs) = Just (r, RightCrumb x l : bs)
goRight (Empty, _) = Nothing

goUp :: Zipper a -> Maybe (Zipper a)
goUp (t, LeftCrumb x r : bs) = Just (Node x t r, bs)
goUp (t, RightCrumb x l : bs) = Just (Node x l t, bs)
goUp (_, []) = Nothing

modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Empty, bs) = (Empty, bs)
modify f (Node x l r, bs) = (Node (f x) l r, bs)

attach :: Tree a -> Zipper a -> Zipper a
attach t (_, bs) = (t, bs)

type ListZipper a = ([a], [a])

goForward :: ListZipper a -> ListZipper a
goForward (x : xs, bs) = (xs, x : bs)

goBack :: ListZipper a -> ListZipper a
goBack (xs, x : bs) = (x : xs, bs)

type Name = String

type Data = String

data FSItem = File Name Data | Folder Name [FSItem] deriving (Show)

myDisk :: FSItem
myDisk =
  Folder
    "root"
    [ File "goat_yelling_like_man.wmv" "baaaaaa",
      File "pope_time.avi" "god bless",
      Folder
        "pics"
        [ File "ape_throwing_up.jpg" "bleargh",
          File "watermelon_smash.gif" "smash!!",
          File "skull_man(scary).bmp" "Yikes!"
        ],
      File "dijon_poupon.doc" "best mustard",
      Folder
        "programs"
        [ File "fartwizard.exe" "10gotofart",
          File "owl_bandit.dmg" "mov eax, h00t",
          File "not_a_virus.exe" "really not a virus",
          Folder
            "source code"
            [ File "best_hs_prog.hs" "main = print (fix error)",
              File "random.hs" "main = print 4"
            ]
        ]
    ]

data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)

type FSZipper = (FSItem, [FSCrumb])

fsUp :: FSZipper -> FSZipper
fsUp (item, FSCrumb name ls rs : bs) = (Folder name (ls ++ [item] ++ rs), bs)

nameIs :: Name -> FSItem -> Bool
nameIs name (Folder folderName _) = name == folderName
nameIs name (File filename _) = name == filename

fsTo :: Name -> FSZipper -> FSZipper
fsTo name (Folder folderName items, bs) =
  let (ls, item : rs) = break (nameIs name) items
   in (item, FSCrumb folderName ls rs : bs)

fsRename :: Name -> FSZipper -> FSZipper
fsRename newName (Folder name items, bs) = (Folder newName items, bs)
fsRename newName (File name dat, bs) = (File newName dat, bs)

fsNewFile :: FSItem -> FSZipper -> FSZipper
fsNewFile item (Folder folderName items, bs) = (Folder folderName $ item : items, bs)

main = do
  -- print $ (freeTree, []) -: goRight -: goLeft -: goUp
  -- let newFocus = (freeTree, []) -: goRight -: goLeft -: modify (\_ -> 'Z')
  -- let farLeft = (freeTree, []) -: goLeft -: goLeft -: goLeft
  -- let newFocus = farLeft -: attach (Node 'Z' Empty Empty)
  -- print newFocus
  -- let xs = [1, 2, 3, 4]
  -- print $ (xs, []) -: goForward -: goForward
  let newFocus = (myDisk, []) -: fsTo "pics" -: fsTo "skull_man(scary).bmp"
  print $ fst newFocus
  print $ (myDisk, []) -: fsTo "pics" -: fsRename "cspi"
  print $ (myDisk, []) -: fsTo "pics" -: fsNewFile (File "heh.jpg" "lol") -: fsUp
