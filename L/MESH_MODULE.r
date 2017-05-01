MESH_MODULE <- setRefClass (

	Class  = "MESH_MODULE",
	fields = c ("lat__1"   = "numeric",
				"lat__2"   = "numeric",
				"lat__3"   = "numeric",
				"lat__4"   = "numeric",
				"lat__5"   = "numeric",
                "lat__6"   = "numeric",
				"lat__100" = "numeric", # 100m メッシュ用
                "lat__50"  = "numeric", # 50m メッシュ用
				"log__1"   = "numeric",
				"log__2"   = "numeric",
				"log__3"   = "numeric",
				"log__4"   = "numeric",
				"log__5"   = "numeric",
                "log__6"   = "numeric",
				"log__100" = "numeric", # 100m メッシュ用
                "log__50"  = "numeric"),# 50m メッシュ用
	methods = list (
		initialize = function ()
		{
			# set options
			options (digits = 15L)
			# set constant
			initFields (lat__1    = 2/3,
						lat__2    = 2/24,
						lat__3    = 2/240,
						lat__4    = 15/3600,
						lat__5    = 7.5/3600,
						lat__6    = 3.75/3600,
                        lat__100  = 2/240/10,
                        lat__50   = 2/240/20,
						log__1    = 1.,
						log__2    = 1/8,
						log__3    = 1/80,
						log__4    = 22.5/3600,
						log__5    = 11.25/3600,
						log__6    = 5.625/3600,
                        log__100  = 1/80/10,
                        log__50   = 1/80/20)
		},
		# +++++++++++++++++++++++++++++++++
		# mesh2bl
		# メッシュから, 緯度経度を算出する
		# +++++++++++++++++++++++++++++++++
		mesh2bl = function (mesh, nth=0)
		{
			mesh <- as.character(mesh)
            if(nth==0)
			    nth  <- .self$mesh_nth (mesh)
			.self$mesh2latlng (mesh, nth)
		},
		mesh_nth = function (mesh)
		{
			bor      <-  c (4, 6, 8, 9, 10, 11, 12)
			nchr     <-  nchar (head (mesh, 1))
			mesh_nth <-  Reduce ('+', vapply (bor, function (x) as.integer (nchr >= x), integer(1)), init=0L)
			stopifnot(mesh_nth >= 1, mesh_nth <= 6, nchr %in% bor, length(table(nchar(mesh), useNA="ifany")) == 1)
			mesh_nth
		},
		mesh2latlng = function (mesh, mesh_nth)
		{
			# 基準メッシュ
			latitude  <- as.integer (substr (mesh, 1, 2)) * lat__1
			lognitude <- as.integer (substr (mesh, 3, 4)) + 100
			# 2次メッシュ
           	if (mesh_nth >= 2) {
           		latitude  <- latitude  + as.integer (substr (mesh, 5, 5)) * lat__2
           		lognitude <- lognitude + as.integer (substr (mesh, 6, 6)) * log__2
           	}
            # 3次メッシュ
           	if (mesh_nth >= 3) {
           		latitude  <- latitude  + as.integer (substr (mesh, 7, 7)) * lat__3
           		lognitude <- lognitude + as.integer (substr (mesh, 8, 8)) * log__3
           	}
			# 4次メッシュ
			if (mesh_nth >= 4)
			{
				latitude  <- latitude  + (as.integer (substr (mesh, 9, 9)) > 2)       * lat__4
				lognitude <- lognitude + (as.integer (substr (mesh, 9, 9)) %% 2 == 0) * log__4
			}
			# 5次メッシュ
			if (mesh_nth >= 5)
			{
				latitude  <- latitude  + (as.integer (substr (mesh, 10, 10)) > 2)       * lat__5
				lognitude <- lognitude + (as.integer (substr (mesh, 10, 10)) %% 2 == 0) * log__5
			}
			# 6次メッシュ
			if (mesh_nth == 6)
			{
				latitude  <- latitude  + (as.integer (substr (mesh, 11, 11)) > 2)       * lat__6
				lognitude <- lognitude + (as.integer (substr (mesh, 11, 11)) %% 2 == 0) * log__6
			}

			return (list (
				lat = latitude,
				log = lognitude
			))

		},
        mesh2latlng__100 = function (mesh)
        {
            # 100mメッシュ用のコンバーター
            # 基準メッシュ
            latitude  <- as.integer (substr (mesh, 1, 2)) * lat__1
            lognitude <- as.integer (substr (mesh, 3, 4)) + 100
            # 2次メッシュ
            latitude  <- latitude  + as.integer (substr (mesh, 5, 5)) * lat__2
            lognitude <- lognitude + as.integer (substr (mesh, 6, 6)) * log__2
            # 3次メッシュ
            latitude  <- latitude  + as.integer (substr (mesh, 7, 7)) * lat__3
            lognitude <- lognitude + as.integer (substr (mesh, 8, 8)) * log__3
            # 100mメッシュ = 3次メッシュの10分の1
            latitude  <- latitude  + as.integer (substr (mesh, 9, 10))  * lat__100
            lognitude <- lognitude + as.integer (substr (mesh, 11, 12)) * log__100

            return (list (
                lat = latitude,
                log = lognitude
            ))
        },
        mesh2latlng__50 = function (mesh)
        {
            # 50mメッシュ用のコンバーター
            # 基準メッシュ
            latitude  <- as.integer (substr (mesh, 1, 2)) * lat__1
            lognitude <- as.integer (substr (mesh, 3, 4)) + 100
            # 2次メッシュ
            latitude  <- latitude  + as.integer (substr (mesh, 5, 5)) * lat__2
            lognitude <- lognitude + as.integer (substr (mesh, 6, 6)) * log__2
            # 3次メッシュ
            latitude  <- latitude  + as.integer (substr (mesh, 7, 7)) * lat__3
            lognitude <- lognitude + as.integer (substr (mesh, 8, 8)) * log__3
            # 50mメッシュ = 3次メッシュの20分の1
            latitude  <- latitude  + as.integer (substr (mesh, 9, 10))  * lat__50
            lognitude <- lognitude + as.integer (substr (mesh, 11, 12)) * log__50

            return (list (
                lat = latitude,
                log = lognitude
            ))
        },
		# +++++++++++++++++++++++++++++++++
		# meshsize_rectangle_coord
		# メッシュの南西の座標値を与えることで, 矩形のポリゴンを作成する
		# +++++++++++++++++++++++++++++++++
		meshsize_rectangle_coord = function (ll_lat, ll_log, mesh_nth) {
			dlat  <- eval(parse(text=paste0("lat__", mesh_nth)))
			dlog  <- eval(parse(text=paste0("log__", mesh_nth)))
			Map(.self$rectangle_coord, ll_lat, ll_log, dlat, dlog)
		},
		rectangle_coord = function (ll_lat, ll_log, dlat, dlog) {
			coord <- c (ll_log, ll_log + dlog, ll_log + dlog, ll_log,        ll_log,
				        ll_lat, ll_lat,        ll_lat + dlat, ll_lat + dlat, ll_lat)
		    matrix(coord, nrow=5, ncol=2)
		},
		# +++++++++++++++++++++++++++++++++
		# bl2mesh
		# 十進数緯度経度から地域メッシュコードを作成する
		# +++++++++++++++++++++++++++++++++
		bl2mesh = function (lat, log)
		{
			d_lat    <- .self$lat2code (lat)
			d_log    <- .self$log2code (log)
			code__4  <- 1 + d_lat$s * 2 + d_log$s * 1
			code__5  <- 1 + d_lat$t * 2 + d_log$t * 1
			code__6  <- 1 + d_lat$u * 2 + d_log$u * 1
			sprintf ("%d%d%d%d%d%d%d%d%d",
					 d_lat$p,
					 d_log$p,
					 d_lat$q,
					 d_log$q,
					 d_lat$r,
					 d_log$r,
					 code__4,
					 code__5,
					 code__6)
		},
		lat2code = function (l)
		{
        	p   <- l   %/% lat__1
        	rst <- l   %%  lat__1
        	q   <- rst %/% lat__2
        	rst <- rst %%  lat__2
        	r   <- rst %/% lat__3
        	rst <- rst %%  lat__3
        	s   <- rst %/% lat__4
        	rst <- rst %%  lat__4
        	t   <- rst %/% lat__5
        	rst <- rst %%  lat__5
        	u   <- rst %/% lat__6
			return (list (p = p, q = q, r = r, s = s, t = t, u = u))
		},
		log2code = function (l)
		{
        	p   <- floor (l - 100)
        	rst <- l - p - 100
        	q   <- rst %/% log__2
        	rst <- rst %%  log__2
        	r   <- rst %/% log__3
        	rst <- rst %%  log__3
        	s   <- rst %/% log__4
        	rst <- rst %%  log__4
        	t   <- rst %/% log__5
        	rst <- rst %%  log__5
        	u   <- rst %/% log__6
        	return (list (p = p, q = q, r = r, s = s, t = t, u = u))
		}
	)
)


