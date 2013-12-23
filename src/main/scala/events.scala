package com.hanhuy.android.voxitoki

import com.hanhuy.android.common.BusEvent
import javax.jmdns.ServiceInfo

/**
 * @author pfnguyen
 */
case class ServiceAdded(svc: ServiceInfo) extends BusEvent

case class ServiceRemoved(svc: ServiceInfo) extends BusEvent

case object ServicesCleared extends BusEvent