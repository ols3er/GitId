# GitId
;; 將緩衝區中的 $GitId$ 替換為： /*  $GitId: FILE,v VERSION YYYY/MM/DD HH:MM:SS USER Exp $ */
;; 其中 VERSION 為該檔案在目前分支的 commit 次數（格式 1.N），USER 取自 git user.name。
;;
;; 使用方式：
;;   M-x gitid-expand     — 立即替換目前 buffer 中所有 $GitId$
;;   M-x gitid-mode       — 切換 minor mode（可搭配 gitid-before-save）
;;   設定 (setq gitid-before-save t) 後，儲存時若在 git repo 會自動：
;;   - 未展開的 $GitId$ → 展開為完整行
;;   - 已展開的 $GitId: FILE,v 1.N ... Exp $ → 版次遞增為 1.(N+1) 並更新日期時間
;;
;; 在程式碼中預先寫好：
;;   /* $GitId$ */
;; 存檔或執行 gitid-expand 後會變成例如：
;;   /* $GitId: 04LST01.c,v 1.2 2026/02/16 03:38:37 ols3 Exp $ */
;; 之後每次存檔會自動把 1.2 變成 1.3、1.3 變成 1.4 …
