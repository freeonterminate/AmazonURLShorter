#Amazon URL Shorter

##概要

Amazon の長い URL をクリップボード格納時に自動的に短くします。

##開発環境

| Item        | Description         |
|-------------|---------------------|
| Environment | Delphi, RAD Studio  |
| Version     | 10.4.2 Sydney       |
| Framework   | FireMonkey          |
| Support OS  | Windows             |

##履歴

2021/05/15  1.00 リリース

##使い方

AmazonURLShorter.dproj を開きます。
ターゲットを Win 64 にして、ビルドします。
実行するとタスクトレイに常駐します。

#### コピー

クリップボードに Amazon の URL がコピーされた時、自動的に短くします。
短くした URL と商品名は自動的にタスクトレイのポップアップメニューに追加されます。
メニューを選ぶとクリップボードにその URL が入ります。
また、Ctrl を押しながらメニューを選ぶとブラウザで商品ページを開きます。

#### 設定

メニュー選択時の動作を決められます

* クリップボードにコピー（デフォルト）
* ブラウザを開く

#### 履歴の数

メニューに追加する履歴の最大数を指定します。
最大を超えると古い順にメニューから削除されます。

#### Windows 起動時に起動

チェックすると Windows 起動時に自動的に起動します。

##Contact
freeonterminate@gmail.com  
http://twitter.com/pik  
      
#LICENSE
Copyright (c) 2021 HOSOKAWA Jun
Released under the MIT license  
http://opensource.org/licenses/mit-license.php
