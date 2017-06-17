module module_mp_driver

    use domain_interface,   only: domain_t
    use module_mp_thompson, only: thompson_init, mp_gt_driver

    logical :: initialized = .false.
contains

    subroutine mp_init(domain)
        implicit none
        type(domain_t), intent(inout) :: domain

        call thompson_init()

        initialized = .true.

    end subroutine mp_init

    subroutine microphysics(domain, dt)
        implicit none
        type(domain_t), intent(inout) :: domain
        real,           intent(in)    :: dt

        if (.not.initialized) call mp_init(domain)

        call mp_gt_driver(  qv=domain%water_vapor%local,            &
                            th=domain%potential_temperature%local,  &
                            qc=domain%cloud_water_mass%local,       &
                            qi=domain%cloud_ice_mass%local,         &
                            ni=domain%cloud_ice_number%local,       &
                            qr=domain%rain_mass%local,              &
                            nr=domain%rain_number%local,            &
                            qs=domain%snow_mass%local,              &
                            qg=domain%graupel_mass%local,           &
                            pii=domain%exner,                       &
                            p=domain%pressure,                      &
                            w=domain%w%local,                       &
                            dz=domain%dz_mass,                      &
                            dt_in=dt,                               &
                            RAINNC=domain%accumulated_precipitation,&
                            SNOWNC=domain%accumulated_snowfall,     &
                            has_reqc=0, has_reqi=0, has_reqs=0,     &
                            ids=domain%ids,ide=domain%ide,          & ! domain dims
                            jds=domain%jds,jde=domain%jde,          &
                            kds=domain%kds,kde=domain%kde,          &
                            ims=domain%ims,ime=domain%ime,          & ! memory dims
                            jms=domain%jms,jme=domain%jme,          &
                            kms=domain%kms,kme=domain%kme,          &
                            its=domain%its,ite=domain%ite,          & ! tile dims
                            jts=domain%jts,jte=domain%jte,          &
                            kts=domain%kts,kte=domain%kte)


    end subroutine microphysics

end module module_mp_driver
